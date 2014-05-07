// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import utexas.aorta.map.{Edge, Road, Turn, Traversable, Position}
import utexas.aorta.sim.EV_Transition
import utexas.aorta.sim.intersections.Ticket
import scala.collection.mutable

import utexas.aorta.common.{Util, cfg, Physics}

abstract class Action
final case class Act_Set_Accel(new_accel: Double) extends Action
final case class Act_Done_With_Route() extends Action

abstract class Behavior(a: Agent) {
  protected var debug_me = false

  // asked every tick after everybody has moved
  def choose_action(): Action
  // only queried when the agent reaches a vertex
  def choose_turn(e: Edge): Turn
  // every time the agent moves to a new traversable
  def transition(from: Traversable, to: Traversable)
  // just for debugging
  def dump_info()

  def set_debug(value: Boolean) {
    debug_me = value
  }
}

// Never speeds up from rest, so effectively never does anything
class IdleBehavior(a: Agent) extends Behavior(a) {
  def choose_action(): Action = Act_Set_Accel(0)
  def choose_turn(e: Edge) = e.next_turns.head
  def transition(from: Traversable, to: Traversable) {}
  def dump_info() {
    Util.log("Idle behavior")
  }
}

// Reactively avoids collisions and obeys intersections by doing a conservative analysis of the
// next few steps.
class LookaheadBehavior(a: Agent, route: Route) extends Behavior(a) {
  override def choose_turn(e: Edge) = a.get_ticket(e).get.turn
  
  override def transition(from: Traversable, to: Traversable) {
    route.transition(from, to)
    a.lc.target_lane = None
  }

  override def dump_info() {
    Util.log("Route-following behavior")
    Util.log(s"Target lane: ${a.lc.target_lane}")
    route.dump_info()
  }

  def choose_action(): Action = {
    a.route.react()
    a.lc.decide_lc()
    val accel = max_safe_accel
    return accel
  }

  // Returns Act_Set_Accel almost always.
  private def max_safe_accel(): Action = {
    // the output.
    var accel_for_stop: Option[Double] = None
    var accel_for_agent: Option[Double] = None
    var min_speed_limit = Double.MaxValue
    var done_with_route = false
    val accel_for_lc_agent = constraint_lc_agent

    // Since we can't react instantly, we have to consider the worst-case of the
    // next tick, which happens when we speed up as much as possible this tick.
    var step = LookaheadStep(a.at, a.kinematic.max_lookahead_dist, 0, a)

    // Verify lookahead doesn't cycle to the same lane twice, since an agent can only hold one
    // ticket per origin lane at a time. Note agents may hit the same road twice in quick succession
    // due to funky geometry and lane-changing.
    val visited = new mutable.HashSet[Edge]()

    // If we don't have to stop for an intersection, keep caring about staying
    // far enough behind an agent. Once we have to stop somewhere, don't worry
    // about agents beyond that point.
    while (step != null && !accel_for_stop.isDefined) {
      step.at.on match {
        case e: Edge => {
          if (visited.contains(e)) {
            throw new Exception(s"Lookahead for $a visited $e twice!")
          }
          visited += e
        }
        case _ =>
      }

      if (!accel_for_agent.isDefined) {
        accel_for_agent = constraint_agent(step)
      }

      if (!accel_for_stop.isDefined) {
        constraint_stop(step) match {
          case Left(constraint) => accel_for_stop = constraint
          case Right(done) => done_with_route = true
        }
      }

      min_speed_limit = math.min(min_speed_limit, constraint_speed_limit(step))

      // Set the next step. If we're stopping here, don't bother -- next_step would fail to find a
      // ticket to figure out where we want to go.
      if (accel_for_stop.isDefined) {
        step = null
      } else {
        step = step.next_step match {
          case Some(s) => s
          case None => null
        }
      }
    }

    return if (done_with_route) {
      Act_Done_With_Route()
    } else {
      val conservative_accel = List(
        accel_for_stop, accel_for_agent, accel_for_lc_agent,
        Some(a.kinematic.accel_to_achieve(min_speed_limit)), Some(a.max_accel)
      ).flatten.min
      //if (debug_me) {
      //  println(s"@ ${a.sim.tick}, ${a.id}'s at ${a.at.dist} with speed ${a.speed} and next accel $conservative_accel")
      //}

      // As the very last step, clamp based on our physical capabilities.
      Act_Set_Accel(math.max(conservative_accel, -a.max_accel))
    }
  }

  // All constraint functions return a limiting acceleration, if relevant
  // Don't plow into people
  private def constraint_agent(step: LookaheadStep): Option[Double] = {
    val follow_agent = if (a.at.on == step.at.on)
                         a.cur_queue.ahead_of(a)
                       else
                         step.at.on.queue.last
    return follow_agent match {
      case Some(other) => {
        Util.assert_ne(a, other)
        val dist_away = if (other.on(a.at.on))
                          other.at.dist - a.at.dist
                        else
                          step.dist_so_far + other.at.dist
        Some(a.kinematic.accel_to_follow(other.kinematic, dist_away))
      }
      case None => None
    }
  }

  // When we're lane-changing, lookahead takes care of the new path. But we still have to pay
  // attention to exactly one other agent: the one in front of us on our old lane. Since we're
  // required to finish lane-changing before reaching the end of the lane, don't have to do full
  // lookahead there.
  private def constraint_lc_agent(): Option[Double] = a.lc.old_lane match {
    case Some(e) => e.queue.ahead_of(a) match {
      case Some(other) =>
        Some(a.kinematic.accel_to_follow(other.kinematic, other.at.dist - a.at.dist))
      case None => None
    }
    case None => None
  }

  // Returns a speed limit, not an acceleration
  private def constraint_speed_limit(step: LookaheadStep) = step.at.on match {
    case e: Edge => route.pick_final_lane(e) match {
      // How many required LCs do we anticipate here? Slown down to increase chances of doing many
      case (target, true) => {
        val num_lcs = math.abs(e.lane_num - target.lane_num)
        // TODO (This is a bit ad-hoc)
        // TODO maybe dont do this while in the turn, only when physically on the lane
        e.speed_limit / (num_lcs + 1)
      }
      case _ => e.speed_limit
    }
    case t: Turn => t.speed_limit
  }

  // Returns an optional acceleration, or 'true', which indicates the agent is totally done.
  private def constraint_stop(step: LookaheadStep): Either[Option[Double], Boolean] = {
    // Request a turn before we need a decision.
    step.at.on match {
      case e: Edge if !route.done(e) => {
        manage_turn(e)
      }
      case _ =>
    }

    // Want to stop in the range [length - end_threshold, length), preferably at that left border
    if (step.dist_left_to_analyze < step.at.dist_left - cfg.end_threshold) {
      return Left(None)
    }

    val can_go: Boolean = step.at.on match {
      // Don't stop at the end of a turn
      case t: Turn => true
      // Stop if we're arriving at destination
      case e: Edge if route.done(e) => false
      // Otherwise, ask the intersection
      case e: Edge => a.get_ticket(e) match {
        case Some(ticket) => ticket.is_approved
        case None => false
      }
    }
    if (can_go) {
      return Left(None)
    }

    // Are we completely done?
    val dist_from_agent_to_end = step.dist_so_far + step.at.dist_left
    val maybe_done = dist_from_agent_to_end <= cfg.end_threshold && a.is_stopped
    return a.at.on match {
      case e: Edge if route.done(e) && maybe_done => Right(true)
      // We want to go the distance that puts us at length - end_threshold. If we're already past
      // that point, then try to cover enough distance to get us to the start of the edge.
      case _ => Left(Some(a.kinematic.accel_to_end(
        math.max(step.dist_so_far, dist_from_agent_to_end - cfg.end_threshold)
      )))
    }
  }

  private def manage_turn(e: Edge) {
    // Schedule a new turn?
    if (!a.get_ticket(e).isDefined && committed_to_lane(e)) {
      val ticket = new Ticket(a, route.pick_turn(e))
      a.add_ticket(ticket)
      e.to.intersection.request_turn(ticket)
    }
    // Getting impatient? This is a late reaction to gridlock.
    a.get_ticket(e) match {
      // TODO this is a policy thing!
      case Some(ticket) if ticket.should_cancel => {
        // Try again. The routing should avoid choices that're filled up, hopefully avoiding gridlock.
        route.optional_reroute(e)
        val next_turn = route.pick_turn(e)
        // Sometimes we pick the same turn here, but the later route could change.
        if (next_turn != ticket.turn) {
          // We don't need to cancel later tickets, because there are none. We only cancel
          // unapproved tickets, and we don't lookahead beyond intersections we can't yet cross.
          // but TODO verify it
          ticket.cancel()
          val replacement = new Ticket(a, next_turn)
          a.add_ticket(replacement)
          e.to.intersection.request_turn(replacement)
        }
      }
      case _ =>
    }
  }

  // Don't necessarily commit to turning from some lane in lookahead
  private def committed_to_lane(e: Edge) =
    if (e == a.at.on)
      a.lc.target_lane match {
        case Some(target) => target == e
        case None => false
      }
    else
      e.other_lanes.size == 1
}

// This is a lazy sequence of edges/turns that tracks distances away from the original spot. This
// assumes no lane-changing: where the agent starts predicting is where they'll end up.
case class LookaheadStep(
  at: Position, dist_left_to_analyze: Double, dist_so_far: Double, a: Agent
) {
  def next_step =
    if (dist_left_to_analyze <= at.dist_left || is_last_step)
      None
    else
      Some(LookaheadStep(
        Position(next_at, 0), dist_left_to_analyze - at.dist_left, dist_so_far + at.dist_left, a
      ))

  private def is_last_step = at.on match {
    case e: Edge => a.route.done(e)
    case _ => false
  }

  private def next_at = at.on match {
    // This is called after manage_turn, which'll guarantee the ticket is present. However, if
    // manage_turn defers the decision (due to LCing), then this method shouldn't be called, since
    // the driver must stop at that intersection.
    case e: Edge => a.get_ticket(e).get.turn
    case t: Turn => t.to
  }
}
