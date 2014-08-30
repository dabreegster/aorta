// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable
import java.io.FileWriter
import scala.io.Source

import utexas.aorta.map.{Graph, Edge, Vertex, Turn}
import utexas.aorta.sim.intersections.{Intersection, Phase, SystemWallets, Policy}
import utexas.aorta.sim.make.{Scenario, MkAgent}
import utexas.aorta.sim.drivers.Agent

import utexas.aorta.common.{Util, cfg, StateWriter, StateReader, Flags, AgentID, Publisher,
                            VertexID, EdgeID, RoadID, Timer}
import utexas.aorta.common.algorithms.PathfindingFailedException
import utexas.aorta.analysis.RerouteCountMonitor

class Simulation(val scenario: Scenario) extends Publisher with AgentManager {
  //////////////////////////////////////////////////////////////////////////////
  // State
  // (All transient, important state is in our constituent traits)

  val graph = scenario.graph
  // Added by a queue that does an in-place check and thinks there could be an
  // issue.
  val active_queues = new mutable.HashSet[Queue]
  // All intersections with agents in them.
  val active_intersections = new mutable.HashSet[Intersection]
  // this represents total "real seconds"
  var tick: Double = 0

  // singleton-like things per sim
  val agent_maps = new mutable.MutableList[AgentMap[_ <: Any]]()
  val system_wallets = new SystemWallets(scenario.sim_config.system_wallet)

  // Track this for stats.
  private var last_real_time = 0.0
  private var steps_since_last_time = 0
  private val routing_monitor = new RerouteCountMonitor(this)

  private lazy val time_limit = Flags.double("--time_limit", Double.MaxValue)
  private lazy val omit = new AgentID(Flags.int("--omit", -1))
  private lazy val should_savestate = Flags.boolean("--savestate", true)

  // TODO I forgot my own pattern for dealing with these simulation-wide singletons. :\
  val latest_delays = new utexas.aorta.contrib.LatestDelay(this)

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def setup(): Simulation = {
    // TODO always do this, and forget this map/sim separation?
    graph.traversables.foreach(t => t.queue = new Queue(t))
    graph.vertices.foreach(v => v.intersection = scenario.make_intersection(v, this))

    // Are we starting for the first time?
    if (tick == 0.0) {
      future_spawn ++= scenario.agents
    } else {
      future_spawn ++= scenario.agents.filter(_.birth_tick > tick)
    }

    return this
  }

  def serialize(w: StateWriter) {
    w.string(scenario.name)
    w.double(tick)
    w.int(finished_count)
    w.int(agents.size)
    agents.foreach(a => w.obj(a))
    w.list_int(ready_to_spawn.map(_.id.int))
    w.lists(graph.traversables.map(_.queue), graph.vertices.map(_.intersection.policy))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def step() {
    tick += cfg.dt_s

    spawn_agents(tick)
    ready_to_spawn = ready_to_spawn.filter(a => !try_spawn(a))

    // If you wanted crazy speedup, disable all but agent stepping and
    // reacting. But that involves trusting that no simulation violations
    // could occur. ;)

    // Queues will lazily start_step, remembering their current state, when
    // they need to.

    // Move agents.
    var active_cnt = 0
    for (a <- agents) {
      if (a.step) {
        active_cnt += 1
      }
      steps_since_last_time += 1
      //println(s"$tick: $a @ ${a.at} doing ${a.speed} due to ${a.target_accel}. ${a.old_lane} and ${a.lanechange_dist_left}, ${a.behavior.target_lane} LC")
      //println(s"  tickets: ${a.tickets}")
    }

    // Just check the ones we need to. Intersections before queues since intersections better
    // account for weird turn conflict cases than winding up with two drivers in the same target
    // lane.
    active_intersections.foreach(i => i.policy.end_step())
    active_queues.foreach(q => q.end_step)
    active_queues.clear()

    // Let agents react to the new world.

    val reap = agents.filter(a => a.react).toSet
    if (reap.nonEmpty) {
      // TODO batch GC.
      reap.foreach(a => a.terminate())
      agents = agents.filter(a => !reap(a))
      finished_count += reap.size
    }

    // Let intersections react to the new world. By doing this after agent
    // steps, we ensure the intersections' temporary state becomes firm.
    for (v <- graph.vertices) {
      v.intersection.policy.react_tick()
    }

    // Record a heartbeat every 1.0s
    if (System.currentTimeMillis - last_real_time >= 1000.0) {
      record_heartbeat(active_cnt)
    }

    if (should_savestate && (tick / cfg.autosave_every).isValidInt && tick > 0.0) {
      savestate()
    }

    publish(EV_Step(tick))
  }

  def multi_step(total_dt: Double) {
    val ticks = total_dt / cfg.dt_s
    Util.assert_eq(ticks.isValidInt, true)
    for (i <- Range(0, ticks.toInt)) {
      step()
    }
  }

  // True if we've correctly promoted into real agents. Does the work of
  // spawning as well.
  private def try_spawn(spawn: MkAgent): Boolean = {
    val r = graph.roads(spawn.start.int)
    val e = r.rightmost
    if (e.queue.can_spawn_now(spawn.start_dist)) {
      // Will we block anybody that's ready?
      val intersection = e.to.intersection
      val will_block =
        if (spawn.route.goal != r.id)
          e.queue.all_agents.find(
            a => a.at.dist < spawn.start_dist && a.wont_block(intersection)
          ).isDefined || e.dont_block
        else
          false
      if (will_block) {
        return false
      } else {
        val a = spawn.make(this)
        if (omit == a.id) {
          Util.log(s"$a would have been created, but omitting")
        } else {
          try {
            a.setup(e, spawn.start_dist)
          } catch {
            case _: PathfindingFailedException => {
              Util.log(s"WARNING: No path from source to destination for $spawn")
              return false
            }
          }
        }
        publish(EV_AgentSpawned(a))
        return true
      }
    } else {
      return false
    }
  }

  private def record_heartbeat(active_cnt: Int) {
    publish(EV_Heartbeat(
      active_cnt, agents.size, ready_to_spawn.size, finished_count, tick, steps_since_last_time,
      routing_monitor
    ))
    last_real_time = System.currentTimeMillis
    steps_since_last_time = 0
    routing_monitor.reset()
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  // TODO move time limit to scenarios?
  def done =
    (agents.isEmpty && ready_to_spawn.isEmpty && future_spawn.isEmpty) ||
    (tick > time_limit)

  def verify_done() {
    Util.assert_eq(agents.isEmpty, true)
    for (v <- graph.vertices) {
      v.intersection.policy.verify_done()
    }
  }

  def savestate(fn: String): String = {
    val t = Timer("savestating")
    val w = Util.writer(fn)
    serialize(w)
    w.done()
    Util.log(s"Savestated to $fn. It took ${t.so_far}s.")
    return fn
  }

  def savestate(): String = {
    return savestate(
      scenario.name.replace("scenarios/", "scenarios/savestate_") + "_" +
      tick.toInt
    )
  }
}

object Simulation {
  // Calls sim.setup()!
  def unserialize(r: StateReader): Simulation = {
    val sim = Scenario.load(r.string).make_sim()
    // Do this before setup so we don't add the wrong spawners
    sim.tick = r.double
    sim.finished_count = r.int
    // Need so queues/intersections are set up.
    sim.setup()
    val num_agents = r.int
    for (i <- Range(0, num_agents)) {
      sim.insert_agent(Agent.unserialize(r, sim))
    }
    val num_ready = r.int
    val ready_ids = Range(0, num_ready).map(_ => new AgentID(r.int)).toSet
    sim.ready_to_spawn ++=
      sim.scenario.agents.filter(a => ready_ids.contains(a.id)).sortBy(_.birth_tick)
    r.int // equal to number of traversables
    for (t <- sim.graph.traversables) {
      Queue.unserialize(t.queue, r, sim)
    }
    r.int // equal to number of vertices
    for (v <- sim.graph.vertices) {
      Policy.unserialize(v.intersection.policy, r, sim)
    }
    return sim
  }
}

trait AgentManager {
  //////////////////////////////////////////////////////////////////////////////
  // State

  var agents: immutable.SortedSet[Agent] = immutable.TreeSet.empty[Agent]
  var ready_to_spawn = new mutable.ListBuffer[MkAgent]()
  val future_spawn = new mutable.PriorityQueue[MkAgent]()
  var finished_count = 0

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def spawn_agents(tick: Double) {
    while (future_spawn.nonEmpty && tick >= future_spawn.head.birth_tick) {
      ready_to_spawn += future_spawn.dequeue
    }
  }

  def insert_agent(a: Agent) {
    Util.assert_eq(agents.contains(a), false)
    agents += a
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  // Only used by the UI, so is O(n) rather than a binary search.
  def get_agent(id: Int) = agents.find(a => a.id.int == id)
  def has_agent(a: Agent) = agents.contains(a)
}

// A map from agent to something else, which supports defaults and knows when
// agents are created or destroyed.
class AgentMap[T](default: T, sim: Simulation) {
  private val mapping = new mutable.HashMap[AgentID, T]()
  sim.agent_maps += this

  def get(id: AgentID): T = mapping.getOrElse(id, default)
  def get(a: Agent): T = get(a.id)
  def put(id: AgentID, value: T) {
    mapping(id) = value
  }
  def values = mapping.values

  def when_created(a: Agent) {}
  def destroy(a: Agent) {
    mapping.remove(a.id)
  }

  // If we load from a save-state, this can be useful.
  def create_from_existing(sim: Simulation) {
    sim.agents.foreach(a => when_created(a))
  }
}
