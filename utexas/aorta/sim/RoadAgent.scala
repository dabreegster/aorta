// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable

import utexas.aorta.map.{Road, Turn}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.common.{cfg, Util, Price}

// Present information about congestion at the road level
abstract class RoadAgent(val r: Road, sim: Simulation) {
  def congested(): Boolean
  // TODO how often must this be called?
  def react() {}

  def congested_now = r.lanes.exists(e => e.queue.is_congested)

  // Worst-case of any constituent lanes
  def freeflow_capacity = r.lanes.map(_.queue.freeflow_capacity).min
  def freeflow_percent_full = r.lanes.map(_.queue.percent_freeflow_full).max

  // Free until 50% freeflow capacity, then $1 per % full. Should range from $0-$50 until
  // congestion.
  // Also, ignore roads with absurdly low capacity. Those're always free.
  def toll =
    if (freeflow_capacity >= 3)
      new Price(math.max(0, freeflow_percent_full - 50))
    else
      new Price(0)

  override def toString = s"Road agent for $r"
}

// Just report the current state of congestion. Subject to oscillation.
class CurrentCongestion(r: Road, sim: Simulation) extends RoadAgent(r, sim) {
  private var last_state = false
  // Since react() is called every tick, might as well cache the current state
  private var current_state = congested_now

  override def congested = current_state
  override def react() {
    super.react()
    current_state = congested_now
    if (congested != last_state) {
      sim.publish(EV_LinkChanged(r, congested))
      last_state = congested
    }
  }
}

// Only flip states congested<->not if the state persists for >= 30s. Has a bias for whatever state
// starts.
class StickyCongestion(r: Road, sim: Simulation) extends RoadAgent(r, sim) {
  private val threshold = 30.0  // seconds
  private var congested_state = false
  private var opposite_since: Option[Double] = None

  override def congested = congested_state

  override def react() {
    super.react()
    // Are we currently the same as we are permanently?
    if (congested_now == congested_state) {
      // Reset any timers that could've changed state
      opposite_since = None
    } else {
      // We're different! How long has it been?
      opposite_since match {
        case Some(start_time) if sim.tick - start_time >= threshold => {
          // State change!
          congested_state = congested_now
          opposite_since = None
          sim.publish(EV_LinkChanged(r, congested_state))
        }
        // Start the timer
        case None => opposite_since = Some(sim.tick)
        case Some(start_time) =>  // Haven't stayed this way long enough yet
      }
    }
  }
}

// Report the most popular state of the last 30s
class MovingWindowCongestion(r: Road, sim: Simulation) extends RoadAgent(r, sim) {
  private val duration = 31.0  // seconds
  private def num_observations = (duration / cfg.dt_s).toInt
  Util.assert_eq(num_observations % 2, 1) // must be odd
  private val last_observations = new mutable.Queue[Boolean]()
  Range(0, num_observations).foreach(_ => last_observations += false)
  private var true_cnt = 0
  private var false_cnt = 30
  private var last_state = false

  override def congested = true_cnt > false_cnt  // == never happens because duration is odd

  // TODO assumes we'll be called every tick... fix
  override def react() {
    super.react()
    last_observations.dequeue() match {
      case true => true_cnt -= 1
      case false => false_cnt -= 1
    }
    val now = congested_now
    last_observations.enqueue(now)
    now match {
      case true => true_cnt += 1
      case false => false_cnt += 1
    }
    if (congested != last_state) {
      sim.publish(EV_LinkChanged(r, congested))
      last_state = congested
    }
  }
}

// When any link in an agent's path changes state, inform them.
class RouteChangeWatcher(sim: Simulation) {
  // Subscribed when the road was congested
  private val subscribers_congested
    = sim.graph.roads.map(r => r -> new mutable.HashSet[Agent]()).toMap
  // Subscribed when the road was clear
  private val subscribers_clear = sim.graph.roads.map(r => r -> new mutable.HashSet[Agent]()).toMap

  sim.listen(classOf[EV_Reroute], _ match {
    case EV_Reroute(a, path, _, _, _, old_path) => {
      for (r <- old_path) {
        subscribers_congested(r) -= a
        subscribers_clear(r) -= a
      }
      for (r <- path) {
        if (r.road_agent.congested) {
          subscribers_congested(r) += a
        } else {
          subscribers_clear(r) += a
        }
      }
    }
  })
  sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, _, to: Turn) => {
      subscribers_congested(to.from.road) -= a
      subscribers_clear(to.from.road) -= a
    }
    case _ =>
  })
  sim.listen(classOf[EV_LinkChanged], _ match { case EV_LinkChanged(r, congested) => {
    if (congested) {
      //subscribers_clear(r).foreach(a => ...)
    } else {
      // Maybe don't tell these guys?
      //subscribers_congested(r).foreach(a => ...)
    }
  }})
}
