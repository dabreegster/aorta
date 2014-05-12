// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import scala.collection.mutable

import utexas.aorta.map.{Edge, Coordinate, Turn, Traversable, Graph, Position, Vertex}
import utexas.aorta.sim.{Simulation, EV_AgentQuit, EV_Breakpoint}
import utexas.aorta.sim.intersections.{Intersection, Ticket}
import utexas.aorta.ui.Renderable

import utexas.aorta.common.{Util, cfg, Physics, StateWriter, StateReader, AgentID, EdgeID,
                            ValueOfTime, Flags, Serializable}

class Agent(
  val id: AgentID, val route: Route, val wallet: Wallet, val sim: Simulation
) extends Ordered[Agent] with Renderable with Serializable
{
  //////////////////////////////////////////////////////////////////////////////
  // Transient state

  var debug_me = false  // TODO public read, private write

  //////////////////////////////////////////////////////////////////////////////
  // State

  var at: Position = null

  // We can only set a target acceleration, which we travel at for the entire duration of timesteps.
  val max_accel = cfg.max_accel
  // TODO max_deaccel too
  var speed: Double = 0.0   // meters/sec
  var target_accel: Double = 0  // m/s^2
  private val behavior = new LookaheadBehavior(this, route)

  // how long has our speed been 0?
  private var idle_since = -1.0

  // keyed by origin lane
  private val tickets = new mutable.HashMap[Edge, Ticket]()

  val lc = new LaneChangingHandler(this, behavior)  // TODO private

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def setup(spawn: Edge, dist: Double) {
    at = spawn.queue.enter(this, dist)
    spawn.queue.allocate_slot()
    sim.insert_agent(this)
    sim.agent_maps.foreach(m => m.when_created(this))
    set_debug(Flags.int("--track", -1) == id.int)
    // Try to do this after most of our state has been set up
    wallet.setup(this)
    route.setup(this)
    if (debug_me) {
      sim.publish(EV_Breakpoint(this))
    }
  }

  def serialize(w: StateWriter) {
    w.int(id.int)
    w.objs(route, wallet, at)
    w.doubles(speed, target_accel)
    w.opts(lc.target_lane.map(_.id.int), lc.old_lane.map(_.id.int))
    w.doubles(lc.lanechange_dist_left, idle_since)
    w.list(tickets.values.toList.sorted)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // Returns true if we move or do anything at all
  def step(): Boolean = {
    lc.start_lc()

    // Do physics to update current speed and figure out how far we've traveled in this timestep.
    // Subtle note: Do this after LCing, else we see the wrong speed
    val new_dist = update_kinematics(cfg.dt_s)

    lc.stop_lc(new_dist)

    // TODO get rid of this short circuit entirely?
    if (is_stopped && target_accel <= 0.0 && new_dist == 0.0) {
      return false
    }

    val start_on = at.on
    val old_dist = at.dist

    idle_since = if (is_stopped && idle_since == -1.0)
                   sim.tick   // we've started idling
                 else if (is_stopped)
                   idle_since   // keep same
                 else
                   -1.0   // we're not idling

    // Check speed limit. Allow a bit of slack.
    Util.assert_le(speed, start_on.speed_limit + cfg.epsilon)

    // Apply this distance. 
    var current_on = start_on
    var current_dist = old_dist + new_dist

    while (current_dist >= current_on.length) {
      if (current_on != start_on && lc.is_lanechanging) {
        throw new Exception(this + " just entered an intersection while lane-changing!")
      }
      current_dist -= current_on.length
      // Are we finishing a turn or starting one?
      val next: Traversable = current_on match {
        case e: Edge => behavior.choose_turn(e)
        case t: Turn => t.to
      }

      // tell the intersection and road agent
      (current_on, next) match {
        case (e: Edge, t: Turn) => {
          t.vert.intersection.enter(get_ticket(e).get)
          e.queue.free_slot()
        }
        case (t: Turn, e: Edge) => {
          // First intersection is a freebie
          if (sim.scenario.agents(id.int).start != t.from.road.id) {
            t.vert.intersection.tollbooth.exit(this, t)
          }
          val ticket = get_ticket(t.from).get
          ticket.done_tick = sim.tick
          remove_ticket(ticket)
          ticket.intersection.exit(ticket)
          sim.publish(ticket.stat)
          // TODO messy.
          // To register for a toll, pick our ideal lane, and the ideal turn from that lane.
          // It may not end up being true that we take that ideal turn.
          if (!route.done(e)) {
            // TODO try to avoid this usage of pick_turn...
            // pick_turn has side-effects, so we try our best to avoid those.
            val next_turn = route.pick_turn(route.pick_final_lane(e)._1, query_only = true)
            e.road.to.intersection.tollbooth.enter(this, next_turn)
          }
        }
      }

      // this lets behaviors make sure their route is being followed
      behavior.transition(current_on, next)
      current_on = next
    }

    // so we finally end up somewhere...
    if (start_on == current_on) {
      val old_dist = at.dist
      at = start_on.queue.move(this, current_dist, old_dist)
      // Also stay updated in the other queue
      lc.old_lane match {
        // our distance is changed since we moved above...
        case Some(lane) => lane.queue.move(this, current_dist, old_dist)
        case None =>
      }
    } else {
      start_on.queue.exit(this, at.dist)
      at = current_on.queue.enter(this, current_dist)
    }

    return new_dist > 0.0
  }

  // Returns true if we're done
  def react(): Boolean = behavior.choose_action match {
    case Act_Set_Accel(new_accel) => {
      // we have physical limits
      Util.assert_le(new_accel.abs, max_accel)
      target_accel = new_accel
      false
    }
    case Act_Done_With_Route() => {
      // No need to exit from the intersection tollbooth; we wouldn't have registered on the last
      // road
      //Util.assert_eq(at.on.asInstanceOf[Edge].road, route.goal)
      // Trust behavior, don't abuse this.
      // (Plus, it doesn't hold for RouteAnalyzer vehicles...)
      Util.assert_eq(speed <= cfg.epsilon, true)
      true
    }
  }

  // returns distance traveled, updates speed. note unit of the argument.
  private def update_kinematics(dt_sec: Double): Double = {
    // Travel at the target constant acceleration for the duration of the
    // timestep, capping off when speed hits zero.
    val dist = Physics.dist_at_constant_accel(target_accel, dt_sec, speed)
    Util.assert_ge(dist, 0.0)
    speed = Physics.update_speed(speed, target_accel, dt_sec)
    return dist
  }

  // Caller must remove this agent from the simulation list
  def terminate() {
    at.on.queue.exit(this, at.dist)
    at.on match {
      case e: Edge => e.queue.free_slot()
      case _ =>
    }
    Util.assert_eq(tickets.isEmpty, true)
    val maker = sim.scenario.agents(id.int)
    sim.publish(EV_AgentQuit(
      this, maker.birth_tick, sim.graph.get_r(maker.start), route.goal, wallet.wallet_type,
      maker.wallet.budget, sim.tick, wallet.budget, wallet.priority
    ))
    sim.agent_maps.foreach(m => m.destroy(this))
  }

  def set_debug(value: Boolean) {
    debug_me = value
    behavior.set_debug(value)
    route.set_debug(value)
    wallet.set_debug(value)
  }

  def add_ticket(ticket: Ticket) {
    Util.assert_eq(ticket.a, this)
    Util.assert_eq(tickets.contains(ticket.turn.from), false)
    tickets += ((ticket.turn.from, ticket))
  }
  def remove_ticket(ticket: Ticket) {
    val removed = tickets.remove(ticket.turn.from)
    Util.assert_eq(removed.get, ticket)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = "Agent " + id
  override def compare(other: Agent) = id.int.compare(other.id.int)
  override def tooltip = List(toString, "Priority " + wallet.priority)
  def debug() {
    Util.log("" + this)
    Util.log_push
    Util.log("At: " + at)
    Util.log("Speed: " + speed)
    Util.log("How long idle? " + how_long_idle)
    Util.log("Max next speed: " + kinematic.max_next_speed)
    Util.log("Stopping distance next: " + Physics.stopping_distance(kinematic.max_next_speed))
    Util.log("Lookahead dist: " + kinematic.max_lookahead_dist)
    Util.log("Dist left here: " + at.dist_left)
    at.on match {
      case e: Edge => {
        Util.log(s"${num_ahead(e)} agents ahead of us in this lane, ${num_ahead(e.leftmost_lane)} in the leftmost lane")
      }
      case _ =>
    }
    Util.log("Tickets:")
    for (ticket <- tickets.values) {
      Util.log(s"  $ticket waiting for ${ticket.how_long_waiting}")
      Util.log("  Gridlock? " + Intersection.detect_gridlock(ticket.turn))
    }
    lc.dump_info()
    behavior.dump_info()
    Util.log_pop()
  }

  def kinematic = Kinematic(at.dist, speed, at.on.speed_limit)

  def how_long_idle = if (idle_since == -1.0)
                        0
                      else
                        sim.tick - idle_since
  def is_stopped = speed <= cfg.epsilon

  def get_ticket(from: Edge) = tickets.get(from)
  // TODO rm this method.
  def all_tickets(i: Intersection) = tickets.values.filter(t => t.intersection == i)
  // If true, we will NOT block when trying to proceed past this intersection
  def wont_block(i: Intersection) = at.on match {
    // We won't block any intersection if we're about to vanish
    case e: Edge if route.done(e) => true
    case _ => tickets.values.find(
      t => t.intersection == i && (t.is_approved || t.is_interruption)
    ).isDefined
  }

  def on_a_lane = at.on match {
    case e: Edge => true
    case t: Turn => false
  }

  def cur_queue = at.on.queue
  // Meaningless unless e is in the same road as the agent
  def num_ahead(e: Edge) = at.on match {
    case edge: Edge if edge.road == e.road =>
      // min() handles when lanes in same road are very different in length
      e.queue.all_in_range(math.min(at.dist, e.length), false, e.length, true).size
    // If we're not there yet, then all of them are ahead!
    case _ => e.queue.agents.size
  }
  def num_behind = at.on.queue.all_in_range(0, true, at.dist, false).size
  def on(t: Traversable) = (at.on, lc.old_lane) match {
    case (ours, _) if ours == t => true
    case (_, Some(l)) if l == t => true
    case _ => false
  }
  def cur_vert = at.on match {
    case e: Edge => e.to
    case t: Turn => t.vert
  }
  def our_lead = at.on.queue.ahead_of(this)
  def our_tail = at.on.queue.behind(this)
  def how_far_away(i: Intersection) = steps_to(at.on, i.v).map(_.length).sum - at.dist
  // Report some number that fully encodes our current choice
  // TODO should be getting turns chosen and LCs done too, to distinguish a few
  // rare cases thatd we'll otherwise blur.
  def characterize_choice = target_accel

  // No lane-changing, uses turns from requested tickets. This means callers can't expect this to
  // reach beyond what lookahead has touched.
  def steps_to(at: Traversable, v: Vertex): List[Traversable] = at match {
    case e: Edge if e.to == v => at :: Nil
    case e: Edge => at :: steps_to(get_ticket(e).get.turn, v)
    case t: Turn => at :: steps_to(t.to, v)
  }
}

object Agent {
  def unserialize(r: StateReader, sim: Simulation): Agent = {
    val a = new Agent(
      new AgentID(r.int), Route.unserialize(r, sim.graph), Wallet.unserialize(r), sim
    )
    a.at = Position.unserialize(r, sim.graph)
    a.speed = r.double
    a.target_accel = r.double
    a.lc.target_lane = r.int match {
      case -1 => None
      case x => Some(sim.graph.edges(x))
    }
    a.lc.old_lane = r.int match {
      case -1 => None
      case x => Some(sim.graph.edges(x))
    }
    a.lc.lanechange_dist_left = r.double
    a.idle_since = r.double
    val num_tickets = r.int
    a.tickets ++= Range(0, num_tickets).map(_ => {
      val t = Ticket.unserialize(r, a, sim.graph)
      (t.turn.from, t)
    })
    // Add ourselves back to a queue
    a.at.on.queue.enter(a, a.at.dist)
    a.lc.old_lane match {
      case Some(e) => e.queue.enter(a, a.at.dist)
      case None =>
    }
    // Add ourselves back to intersections
    for (ticket <- a.tickets.values if ticket.is_approved) {
      ticket.intersection.policy.unserialize_accepted(ticket)
    }
    a.at.on match {
      case t: Turn => t.vert.intersection.enter(a.get_ticket(t.from).get)
      case _ =>
    }
    // TODO other things in setup(), like setting debug_me?
    a.wallet.setup(a)
    a.route.setup(a)
    return a
  }
}
