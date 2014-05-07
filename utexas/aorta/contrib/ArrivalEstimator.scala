// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import utexas.aorta.sim.{Simulation, EV_TurnStarted, EV_TurnApproved}
import utexas.aorta.sim.drivers.{Agent, Kinematic}
import utexas.aorta.map.Vertex
import utexas.aorta.common.{Util, AgentID, Physics, cfg}

import scala.collection.mutable

class ArrivalEstimator(sim: Simulation) {
  private val estimates = new mutable.HashMap[(Agent, Vertex), DelayModel.ModelResult]()

  sim.listen(classOf[EV_TurnApproved], _ match { case EV_TurnApproved(ticket) => {
    // TODO fails if ticket.a isn't on edge before turn
    val lane = ticket.turn.from
    val all_ahead = lane.queue.all_ahead_of(ticket.a.at.dist).reverse
    Util.assert_eq(all_ahead.head, ticket.a)
    // TODO 2nd speed limit should be for THEIR ticket
    val models = all_ahead.map(a => new DelayModel.AgentModel(
      a.id, a.kinematic, ticket.turn.from.speed_limit, ticket.turn.speed_limit, a.target_accel,
      a.route.done(lane)
    ))
    estimates((ticket.a, ticket.intersection.v)) = DelayModel.simulate(
      models, lane.length, ticket.a.debug_me, ticket.a.sim.tick
    )
  }})
  sim.listen(classOf[EV_TurnStarted], _ match { case EV_TurnStarted(ticket) => {
    val a = ticket.a
    val estimate = estimates.remove((a, ticket.intersection.v)).get
    val predict_time = estimate.arrival_time
    val predict_speed = estimate.arrival_speed
    val actual_time = a.sim.tick
    val actual_speed = a.speed

    println(s"$a time (actual $actual_time, should $predict_time), speed (actual $actual_speed, should $predict_speed)")
    if (math.abs(actual_time - predict_time) > 1.0 || actual_speed != predict_speed) {
      println("    !!! significant error")
    }
  }})
}

object DelayModel {
  class AgentModel(
    val id: AgentID, initial_state: Kinematic, speed_limit1: Double, speed_limit2: Double,
    initial_accel: Double, stop_at_end: Boolean
  ) {
    private var kinematic = initial_state
    private var target_accel: Double = initial_accel
    def dist = kinematic.dist
    def speed = kinematic.speed
    def next_accel = target_accel

    def step() {
      val moved_dist = Physics.dist_at_constant_accel(target_accel, cfg.dt_s, kinematic.speed)
      Util.assert_ge(moved_dist, 0.0)
      val new_speed = math.max(0.0, speed + (target_accel * cfg.dt_s))
      kinematic = kinematic.copy(dist = dist + moved_dist, speed = new_speed)
    }

    // Returns true if still alive
    def react(leader: Option[AgentModel], length: Double, debug: Boolean): Boolean = {
      val dist_till_end = length - dist

      // Vanish?
      if (stop_at_end && dist_till_end <= cfg.end_threshold && speed <= cfg.epsilon) {
        return false
      }

      // What speed limit do we follow?
      val limit =
        if (dist + kinematic.max_lookahead_dist >= length)
          math.min(speed_limit1, speed_limit2)
        else
          speed_limit1

      val accel_to_end =
        if (!stop_at_end || dist + kinematic.max_lookahead_dist < length - cfg.end_threshold)
          None
        else
          Some(kinematic.accel_to_end(math.max(0, dist_till_end - cfg.end_threshold)))

      val accel_for_agent = leader.map(
        follow => kinematic.accel_to_follow(follow.kinematic, follow.dist - kinematic.dist)
      )

      if (debug) {
        println(s"    : for $id we have... $accel_to_end, $accel_for_agent, limit $limit")
      }
      target_accel = List(
        accel_to_end, accel_for_agent, Some(kinematic.accel_to_achieve(limit)),
        Some(cfg.max_accel)
      ).flatten.min
      target_accel = math.max(target_accel, -cfg.max_accel)
      return true
    }
  }
  case class ModelResult(arrival_time: Double, arrival_speed: Double)

  // agents.head has least distance, aka, is at the back, and is the agent we describe in the result
  def simulate(initial_agents: List[AgentModel], length: Double, debug: Boolean, time: Double): ModelResult = {
    var dt = time
    var agents = initial_agents
    while (agents.head.dist < length) {
      dt += cfg.dt_s
      // step before react
      agents.foreach(a => a.step())
      val alive_agents = new mutable.ListBuffer[AgentModel]()
      for ((a, leader) <- agents.zipAll(agents.tail, null, null)) {
        if (a.react(Option(leader), length, debug)) {
          // Don't ever get rid of our person
          if (a.dist < length || a == agents.head) {
            alive_agents += a
          }
        }
      }
      agents = alive_agents.toList

      if (debug) {
        println(s"it's $dt")
        for (a <- agents) {
          println(s"  for ${a.id} at ${a.dist}, speed is ${a.speed} and next accel is ${a.next_accel}")
        }
      }
    }
    return ModelResult(dt, agents.head.speed)
  }
}
