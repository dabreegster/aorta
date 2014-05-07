// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common

// All these assume cfg.max_accel, which will be per-car in the future.
object Physics {
  // Capped when speed goes negative.
  def dist_at_constant_accel(accel: Double, time: Double, initial_speed: Double): Double = {
    // Don't deaccelerate into going backwards, just cap things off.
    val actual_time = if (accel >= 0)
                        time
                      else
                        math.min(time, -1 * initial_speed / accel)
    return (initial_speed * actual_time) + (0.5 * accel * (actual_time * actual_time))
  }

  def update_speed(speed: Double, accel: Double, time: Double) =
    math.max(0.0, speed + (accel * time))

  // TODO this gets a bit more conservative when cars have different
  // accelerations. This is hinged on the fact that lookahead works. Agents
  // can't enter e faster than its speed limit, so we have to reason about how
  // far they could possibly go.
  def worst_entry_dist(lim: Double): Double = {
    val accel = cfg.max_accel
    val stopping_dist = dist_at_constant_accel(-accel, lim / accel, lim)
    return (lim * cfg.dt_s) + stopping_dist
  }

  // stopping time comes from v_f = v_0 + a*t
  // negative accel because we're slowing down.
  def stopping_distance(speed: Double) = dist_at_constant_accel(
    -cfg.max_accel, speed / cfg.max_accel, speed
  )

  // We'll be constrained by the current edge's speed limit and maybe other
  // stuff, but at least this speed lim.
  def max_next_accel(speed: Double, limit: Double) =
    math.min(cfg.max_accel, (limit - speed) / cfg.dt_s)
  def max_next_speed(speed: Double, limit: Double) =
    speed + (max_next_accel(speed, limit) * cfg.dt_s)
  def max_next_dist(speed: Double, limit: Double) =
    dist_at_constant_accel(max_next_accel(speed, limit), cfg.dt_s, speed)
  def max_next_dist_plus_stopping(speed: Double, limit: Double) =
    max_next_dist(speed, limit) + stopping_distance(max_next_speed(speed, limit))
  def max_lookahead_dist(speed: Double, limit: Double) =
    max_next_dist_plus_stopping(speed, limit)

  def min_next_speed(speed: Double) =
    math.max(0.0, speed + (cfg.dt_s * -cfg.max_accel))
  def min_next_dist(speed: Double) =
    dist_at_constant_accel(-cfg.max_accel, cfg.dt_s, speed)
  def min_next_dist_plus_stopping(speed: Double) =
    min_next_dist(speed) + stopping_distance(min_next_speed(speed))
  
  def accel_to_achieve(target_speed: Double, speed: Double) =
    (target_speed - speed) / cfg.dt_s

  // d = (v_i)(t) + (1/2)(a)(t^2), solved for a
  def accel_to_cover(dist: Double, speed: Double) =
    (2 * (dist - (speed * cfg.dt_s)) / (cfg.dt_s * cfg.dt_s))

  // To stop in one time-step, that is. From v_f = v_i + at
  def accel_to_stop(speed: Double) = (-1 * speed) / cfg.dt_s

  // to meters/sec
  def mph_to_si(r: Double) = r * 0.44704

  // How much time to cover >= target_dist as fast as possible with initial/max speed?
  def simulate_steps(target_dist: Double, initial_speed: Double, max_speed: Double): Double = {
    var dist = 0.0
    var speed = initial_speed
    var dt = 0.0
    while (dist <= target_dist) {
      dt += cfg.dt_s
      val accel = math.min(cfg.max_accel, (max_speed - speed) / cfg.dt_s)
      dist += dist_at_constant_accel(accel, cfg.dt_s, speed)
      speed += accel * cfg.dt_s
    }
    return dt
  }
}
