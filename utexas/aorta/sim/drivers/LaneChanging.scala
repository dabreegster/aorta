// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import utexas.aorta.map.{Edge, Turn}

import utexas.aorta.common.{cfg, Physics, Util}

// Organize all the lane-changing checks/actions here
class LaneChangingHandler(a: Agent, behavior: Behavior) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  // TODO make these private and do our own serialization.
  // old_lane is where we're shifting from. we immediately warp into the target lane.
  var old_lane: Option[Edge] = None
  var lanechange_dist_left: Double = 0

  // None indicates we need to decide. If it's equal to the agent's current lane, then no
  // lane-changing needed.
  var target_lane: Option[Edge] = None

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // Called in step() before we move
  def start_lc() {
    // If we're not already lane-changing, should we start?
    if (!is_lanechanging && a.on_a_lane) {
      target_lane match {
        case Some(lane) if lane != a.at.on && safe_to_lc(lane) => {
          // We have to cover a fixed distance to lane-change.
          lanechange_dist_left = cfg.lanechange_dist
          old_lane = Some(a.at.on.asInstanceOf[Edge])

          // Immediately enter the target lane
          behavior.transition(a.at.on, lane)
          a.at = lane.queue.enter(a, a.at.dist)
          lane.queue.allocate_slot()
        }
        case _ =>
      }
    }
  }

  def stop_lc(new_dist: Double) {
    // Currently lane-changing?
    old_lane match {
      case Some(lane) => {
        lanechange_dist_left -= new_dist
        if (lanechange_dist_left <= 0) {
          // Done! Leave the old queue
          lane.queue.exit(a, a.at.dist)
          lane.queue.free_slot()

          // Return to normality
          old_lane = None
          lanechange_dist_left = 0
        }
      }
      case None =>
    }
  }

  def decide_lc() {
    // Do we want to lane-change?
    (target_lane, a.at.on) match {
      case (None, e: Edge) => target_lane = Some(pick_target_lane)
      case _ =>
    }

    // Give up one lane-changing and settle on a turn?
    (target_lane, a.at.on) match {
      case (Some(target), cur_lane: Edge) if target != cur_lane => {
        // TODO left off here.
        // No room? Fundamentally impossible
        // Somebody in the way? If we're stalled and somebody's in the way,
        // we're probably waiting in a queue. Don't waste time hoping, grab a
        // turn now.
        // TODO call without_blocking too
        // TODO in room_to_lc, dont need tick ahead stuff.
        if (!room_to_lc(target) || (a.is_stopped && !can_lc_without_crashing(target))) {
          target_lane = Some(cur_lane)
        }
      }
      case _ =>
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def is_lanechanging = old_lane.isDefined

  def dump_info() {
    if (is_lanechanging) {
      Util.log(s"Lane-changing from ${old_lane.get}. $lanechange_dist_left to go!")
    } else {
      Util.log("Not lane-changing")
    }
  }

  private def pick_target_lane(): Edge = {
    if (target_lane.isDefined) {
      throw new IllegalStateException("Target lane already set")
    }
    val base = a.at.on.asInstanceOf[Edge]

    // If we already committed to a turn, then don't lane-change.
    if (a.get_ticket(base).isDefined) {
      return base
    } else {
      val goal = a.route.pick_final_lane(base)._1
      return base.adjacent_lanes.minBy(choice => math.abs(choice.lane_num - goal.lane_num))
    }
  }

  // Just see if we have enough static space to pull off a lane-change.
  private def room_to_lc(target: Edge): Boolean = {
    // Worry about the shorter lane, to be safe
    val min_len = math.min(a.at.on.length, target.length)

    // Finish lane-changing before reaching the intersection
    if (a.at.dist + cfg.lanechange_dist + cfg.end_threshold >= min_len) {
      return false
    }

    // Furthermore, we probably have to stop for the intersection, so be sure we
    // have enough room to do that.
    // This is confusing, but we're called in two places, the most important of
    // which is step(), right before an acceleration chosen in the previous tick
    // will be applied. The behavior chose that acceleration assuming we'd be in
    // the old lane, not the new. TODO redo the reaction here?
    // So we have to apply what will happen next...

    val initial_speed = a.speed
    val final_speed = Physics.update_speed(initial_speed, a.target_accel, cfg.dt_s)
    val dist = Physics.dist_at_constant_accel(a.target_accel, cfg.dt_s, initial_speed)
    val our_max_next_speed = final_speed + (Physics.max_next_accel(final_speed, a.at.on.speed_limit) * cfg.dt_s)

    if (a.at.dist + dist + Physics.stopping_distance(our_max_next_speed) >= min_len) {
      return false
    }
    
    return true
  }

  // Would we cut anybody off if we LC in front of them?
  private def can_lc_without_blocking(target: Edge): Boolean = {
    // TODO is it ok to cut off somebody thats done with their route?
    val intersection = target.to.intersection
    return !target.queue.all_agents.find(
      agent => agent.at.dist < a.at.dist && agent.wont_block(intersection)
    ).isDefined && !target.dont_block
  }

  private def can_lc_without_crashing(target: Edge): Boolean = {
    // We don't want to merge in too closely to the agent ahead of us, nor do we
    // want to make somebody behind us risk running into us. So just make sure
    // there are no agents in that danger range.
    // TODO assumes all vehicles the same. not true forever.

    val initial_speed = a.speed
    val final_speed = math.max(0.0, initial_speed + (a.target_accel * cfg.dt_s))
    val this_dist = Physics.dist_at_constant_accel(a.target_accel, cfg.dt_s, initial_speed)

    // If we see somebody in front of us, we could slam on our brakes. Would it be OK?
    val ahead_dist = cfg.follow_dist + Physics.min_next_dist_plus_stopping(final_speed)
    // For behind, assume somebody behind us is going full speed. Give them time
    // to stop fully and not hit where we are now (TODO technically should
    // account for the dist we'll travel, but hey, doesnt hurt to be safe...
    // Don't forget they haven't applied their step this turn either!
    val behind_dist = (target.speed_limit * cfg.dt_s) + cfg.follow_dist + Physics.max_next_dist_plus_stopping(target.speed_limit, target.speed_limit)

    // TODO +dist for behind as well, but hey, overconservative doesnt hurt for
    // now...
    val nearby = target.queue.all_in_range(
      a.at.dist - behind_dist, true, a.at.dist + this_dist + ahead_dist, true
    )
    return nearby.isEmpty
  }

  private def safe_to_lc(target: Edge): Boolean = {
    val e = a.at.on.asEdge
    if (e.road != target.road) {
      throw new Exception(s"$a wants to lane-change across roads")
    }
    if (math.abs(target.lane_num - e.lane_num) != 1) {
      throw new Exception(s"$a wants to skip lanes when lane-changing")
    }

    if (!room_to_lc(target)) {
      return false
    }

    // Does the target queue have capacity? Don't cause gridlock!
    if (!target.queue.slot_avail) {
      return false
    }
    if (!can_lc_without_blocking(target)) {
      return false
    }

    // If there's somebody behind us on the target, or if we're beyond the
    // worst-case entry distance, we don't have to worry about drivers on other
    // roads about to take turns and fly into the new lane. But if we are
    // concerned, just ask the intersection!
    if (!target.queue.closest_behind(a.at.dist).isDefined &&
        a.at.dist <= a.at.on.worst_entry_dist + cfg.follow_dist)
    {
      // Smart look-behind: the intersection knows.
      val beware = target.from.intersection.policy.approveds_to(target)
      // TODO for now, if theres any -- worry. could do more work using the
      // below to see if they'll wind up too close, though.
      if (beware.nonEmpty) {
        return false
      }
    }

    if (!can_lc_without_crashing(target)) {
      return false
    }

    return true
  }
}
