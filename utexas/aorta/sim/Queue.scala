// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

// I'd love to use scala's treemap, but it doesnt support higher/lowerkey.
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.mutable
import Function.tupled

import utexas.aorta.map.{Edge, Traversable, Position}
import utexas.aorta.sim.drivers.Agent

import utexas.aorta.common.{Util, cfg, Physics, StateWriter, StateReader, Serializable}

// Reason about collisions on edges and within individual turns.
class Queue(t: Traversable) extends Serializable {
  //////////////////////////////////////////////////////////////////////////////
  // State

  // Descending by distance: the front of traversable has the greatest distance.
  val agents = new java.util.TreeMap[Double, Agent]()
  private var last_tick = -1.0              // last observed
  // to verify no collisions occurred in a step
  private val prev_agents = new mutable.TreeSet[Agent]()

  // Maintain this to determine if it's safe to LC or turn to a lane. If we
  // over-subscribe, agents may block the intersection. For edges only, not
  // turns.
  private var avail_slots = capacity

  // Round down. How many drivers max could squish together here? Minimum 1,
  // short edges just support 1.
  lazy val capacity = math.max(1, math.floor(t.length / separation_dist).toInt)

  //////////////////////////////////////////////////////////////////////////////
  // Meta
  
  def serialize(w: StateWriter) {
    // Agents will add themselves back to us.
    w.int(avail_slots)
    // TODO some of these feel transient, but eh.
    w.double(last_tick)
    w.list_int(prev_agents.map(_.id.int).toList)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def allocate_slot() {
    Util.assert_gt(avail_slots, 0)
    avail_slots -= 1
  }

  // It's the client's responsability to call this. Agents could skip short
  // lanes entirely with big time-steps, sometimes, so calling only in exit()
  // isn't sufficient.
  def free_slot() {
    Util.assert_lt(avail_slots, capacity)
    avail_slots += 1
  }

  // Called lazily.
  def start_step(a: Agent) {
    if (last_tick != a.sim.tick) {
      prev_agents.clear()
      prev_agents ++= all_agents
      last_tick = a.sim.tick
    }
  }

  // Check for collisions by detecting abnormal changes in ordering.
  def end_step(): Unit = {
    // TODO this is inefficient.
    // TODO if an agent ever looped around to the same edge again in one step,
    // this breaks badly.
    // TODO likewise, problems if an agent quickly lane-changed out, passed
    // someone, then back in.

    // Everything's fine.
    if (agents.isEmpty) {
      return
    }

    // TODO more efficiently?
    val alist = agents.values.toList
    // TODO check keys match value.at.dist

    // Since we allow lane-changing, some funny things could happen. So first
    // just check that the order of the distances matches the order of the
    // queue.
    if (!alist.zip(alist.tail).forall(tupled((a1, a2) => a1.at.dist > a2.at.dist))) {
      throw new Exception(
        s"Agents out of order on $t: " + alist.map(a => f"${a.id} at ${a.at.dist}%.2f")
      )
    }

    // Make sure nobody's crowding anybody else.
    for ((a1, a2) <- alist.zip(alist.tail)) {
      if (a1.at.dist < a2.at.dist + cfg.follow_dist) {
        Util.log(s"It's ${a1.sim.tick}")
        Util.log(s"LCing? ${a1.lc.old_lane} (${a1.lc.lanechange_dist_left}), ${a2.lc.old_lane} (${a2.lc.lanechange_dist_left})")
        throw new Exception(
          s"$a2 too close to $a1 on $t (" + (a1.at.dist - a2.at.dist) + ")"
        )
      }
    }

    // Now we just want to make sure that all of the agents here last tick are
    // in the same order. If some left, that's fine.
    val old_crowd = alist.filter(a => prev_agents.contains(a))

    if (old_crowd.size > 1) {
      // Since we know the ordering of the distances matches the ordering of the
      // queue from the first check, it suffices to check the ordering of the
      // distances in this list.
      if (!old_crowd.zip(old_crowd.tail).forall(tupled((a1, a2) => a1.at.dist > a2.at.dist))) {
        throw new Exception(s"Agents swapped positions on $t")
      }
    }
  }

  def enter(a: Agent, dist: Double): Position = {
    // Just find our spot.

    start_step(a)  // lazily, if needed

    Util.assert_eq(agents.get(-dist), null)

    // Use -dist to make highest dist first and avoid comparator junk.
    agents.put(-dist, a)

    // If we're not entering at the end of the queue, something _could_ be odd,
    // so check it.
    if (closest_behind(dist).isDefined) {
      a.sim.active_queues += this
    }

    return Position(t, dist)
  }

  def exit(a: Agent, old_dist: Double) {
    start_step(a)  // lazily, if needed

    Util.assert_eq(agents.get(-old_dist), a)

    // Makes serialization nicer if we load a state with an agent who terminates
    prev_agents -= a

    // We should leave from the front of the queue generally, unless
    // lane-changing
    if (agents.firstEntry.getValue != a) {
      a.sim.active_queues += this
    }

    agents.remove(-old_dist)
  }

  def move(a: Agent, new_dist: Double, old_dist: Double): Position = {
    Util.assert_ge(new_dist, old_dist)
    // TODO more efficiently?
    exit(a, old_dist)
    return enter(a, new_dist)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  private def wrap_option(entry: java.util.Map.Entry[Double, Agent]) =
    if (entry != null)
      Some(entry.getValue)
    else
      None

  override def toString = s"Queue of $t"

  def slot_avail = avail_slots > 0
  def is_full = !slot_avail
  def percent_avail = avail_slots.toDouble / capacity.toDouble * 100.0
  def percent_full = 100.0 - percent_avail
  def slots_filled = capacity - avail_slots
  def isEmpty = agents.isEmpty

  // This should be tuned carefully. A queue with only 3 spots isn't really
  // congested if they're all filled.
  // TODO but then theres cases of gridlock because all the full queues are <= 5 capacity. really
  // want to ask if people there have been stuck for a long time...
  def is_congested = capacity > 5 && percent_full >= 80.0

  // TODO this is way over-conservative. based on accel_to_follow, every
  // negative term in delta_dist, from speed 0.
  def separation_dist =
    cfg.follow_dist + Physics.max_next_dist_plus_stopping(0.0, t.speed_limit) +
    Physics.max_next_dist(0.0, t.speed_limit)
  // How many people can travel at the speed limit comfortably? This is generous, since cars are
  // planning for a worst-case where the leader slams on their brakes
  def freeflow_capacity =
    math.max(1, math.floor(t.length / (t.speed_limit * cfg.dt_s + cfg.follow_dist)).toInt)
  // Easily can be >100% when we're congested
  def percent_freeflow_full = 100.0 * (slots_filled.toDouble / freeflow_capacity.toDouble)

  def head = wrap_option(agents.firstEntry)
  def last = wrap_option(agents.lastEntry)

  def all_agents = agents.values.toSet

  def ahead_of(a: Agent) = closest_ahead(a.at.dist)
  def behind(a: Agent) = closest_behind(a.at.dist)
  def closest_behind(dist: Double) = wrap_option(agents.higherEntry(-dist))
  def closest_ahead(dist: Double) = wrap_option(agents.lowerEntry(-dist))
  // Gives farthest agent first
  def all_ahead_of(dist: Double) = all_in_range(dist, true, t.length, true).values.toList
  def all_in_range(from: Double, from_inclusive: Boolean, to: Double, to_inclusive: Boolean) =
    agents.subMap(-to, to_inclusive, -from, from_inclusive)

  // The real-time spawning magic is really quite simple if worst_entry_dist and
  // lookahead work.
  def can_spawn_now(dist: Double): Boolean = {
    if (is_full) {
      return false
    }

    var safe = true
    // Find the first agent that makes us conclude there's a problem or we're
    // truly safe. This closure yields true when it wants to short-circuit.
    agents.descendingMap.values.find(a => {
      if (dist > a.at.dist) {
        // don't spawn in front of somebody who can't stop
        val bad_dist = cfg.follow_dist + a.kinematic.max_next_dist_plus_stopping
        if (dist - a.at.dist <= bad_dist) {
          safe = false
          true
        } else {
          // keep looking
          false
        }
      } else {
        // don't spawn too close behind somebody
        if (a.at.dist - dist <= cfg.follow_dist) {
          safe = false
        } else {
          safe = true
        }
        true
      }
    })
    return safe
  }
}

object Queue {
  def unserialize(queue: Queue, r: StateReader, sim: Simulation) {
    queue.avail_slots = r.int
    queue.last_tick = r.double
    queue.prev_agents ++= Range(0, r.int).map(_ => sim.get_agent(r.int).get)
  }
}
