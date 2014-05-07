// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import utexas.aorta.common.{Util, cfg, Physics}

case class Kinematic(dist: Double, speed: Double, speed_limit: Double) {
  // Math shortcuts
  def max_lookahead_dist = Physics.max_lookahead_dist(speed, speed_limit)
  def accel_to_achieve(target: Double) = Physics.accel_to_achieve(target, speed)
  def max_next_dist_plus_stopping = Physics.max_next_dist_plus_stopping(speed, speed_limit)
  def max_next_dist = Physics.max_next_dist(speed, speed_limit)
  def min_next_dist = Physics.min_next_dist(speed)
  def max_next_speed = Physics.max_next_speed(speed, speed_limit)

  def accel_to_follow(follow: Kinematic, dist_from_them_now: Double): Double = {
    val us_worst_dist = max_next_dist_plus_stopping
    val most_we_could_go = max_next_dist
    val least_they_could_go = follow.min_next_dist

    // TODO this optimizes for next tick, so we're playing it really
    // conservative here... will that make us fluctuate more?
    val projected_dist_from_them = dist_from_them_now - most_we_could_go + least_they_could_go
    val desired_dist_btwn = us_worst_dist + cfg.follow_dist

    // Positive = speed up, zero = go their speed, negative = slow down
    val delta_dist = projected_dist_from_them - desired_dist_btwn

    // Try to cover whatever the distance is
    return Physics.accel_to_cover(delta_dist, speed)
  }

  // Find an accel to travel want_dist and wind up with speed 0.
  def accel_to_end(want_dist: Double): Double = {
    Util.assert_ge(want_dist, 0)

    if (want_dist == 0.0) {
      // Just stop.
      return Physics.accel_to_stop(speed)
    }

    // d = (v_1)(t) + (1/2)(a)(t^2)
    // 0 = (v_1) + (a)(t)
    // Eliminating time yields the formula for accel below. This same accel should be applied for
    // t = -v_1 / a, which is possible even if that's not a multiple of dt_s since we're
    // decelerating to rest.
    val normal_case = (-1 * speed * speed) / (2 * want_dist)
    val required_time = -speed / normal_case
    if (!required_time.isNaN) {
      return normal_case
    }

    // We have to accelerate so that we can get going, but not enough so
    // that we can't stop. Do one tick of acceleration, one tick of
    // deacceleration at that same rate. If the required acceleration is then too high, we'll cap
    // off and trigger a normal case next tick.
    // Want (1/2)(a)(dt^2) + (a dt)dt - (1/2)(a)(dt^2) = want_dist
    return want_dist / (cfg.dt_s * cfg.dt_s)
  }
}
