// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.RouterType

import utexas.aorta.common.{Util, Price, Physics}
import utexas.aorta.common.algorithms.{AStar, Pathfind, PathResult}

abstract class Router(graph: Graph) {
  protected var debug_me = false

  def router_type: RouterType.Value
  // Includes 'from' as the first step
  def path(spec: Pathfind): PathResult
  def path(from: Road, to: Road): PathResult = path(Pathfind(start = from, goal = to))

  // TODO messy to include this jump, but hard to pipe in specific params...
  def setup(a: Agent) {}

  def set_debug(value: Boolean) {
    debug_me = value
  }
}

class FixedRouter(graph: Graph, initial_path: List[Road]) extends Router(graph) {
  override def router_type = RouterType.Fixed
  override def path(spec: Pathfind): PathResult = {
    Util.assert_eq(spec.start, initial_path.head)
    Util.assert_eq(spec.goal, initial_path.last)
    return PathResult(initial_path, Map(), 0)
  }
}

// Score is a pair of doubles
abstract class AbstractPairAstarRouter(graph: Graph) extends Router(graph) {
  override def path(spec: Pathfind) = AStar.path(transform(spec))

  protected def transform(spec: Pathfind) = spec
}

// No guess for cost, straight-line distance at 1m/s for freeflow time
trait SimpleHeuristic extends AbstractPairAstarRouter {
  private val max_speed = Physics.mph_to_si(80.0)  // TODO put in cfg
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_heuristic = (state: Road) => (0.0, state.end_pt.dist_to(spec.goal.end_pt) / max_speed)
  )
  // Alternate heuristics explore MUCH less states, but the oracles are too
  // pricy. (CH, Dijkstra table of distances)
  // TODO make a fast mode that doesnt divide by speed limit. Suboptimal paths.
}

class FreeflowRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Unusable
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) => (0, next.freeflow_time)
  )
}

// Cost for each step is (dollars, time)
trait TollAndTimeCost extends AbstractPairAstarRouter {
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (next.road_agent.toll.dollars, next.freeflow_time)
  )
}

// Score is (number of congested roads, total freeflow time)
class CongestionRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Congestion
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (Util.bool2binary(next.road_agent.congested), next.freeflow_time)
  )
}

// Score is (max congestion toll, total freeflow time)
class DumbTollRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic with TollAndTimeCost
{
  override def router_type = RouterType.DumbToll
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    add_cost = (a: (Double, Double), b: (Double, Double)) => (math.max(a._1, b._1), a._2 + b._2)
  )
}

// Score is (number of toll violations, total freeflow time)
// We have a max_toll we're willing to pay, so we try to never pass through a road with that toll
class TollThresholdRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic
{
  private var max_toll: Price = new Price(-1)

  override def setup(a: Agent) {
    max_toll = new Price((a.wallet.priority * 100).toInt)
  }

  override def router_type = RouterType.TollThreshold
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (Util.bool2binary(next.road_agent.toll.dollars > max_toll.dollars), next.freeflow_time)
  )
}

// Score is (sum of tolls, total freeflow time). The answer is used as the "free" baseline with the
// least cost to others.
class SumTollRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic with TollAndTimeCost
{
  override def router_type = RouterType.SumToll
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (next.road_agent.toll.dollars, next.freeflow_time)
  )
}

object TollboothRouter {
  var toll_weight = 0.1
}

class TollboothRouter(graph: Graph) extends AbstractPairAstarRouter(graph) {
  // TODO cache this among drivers!
  private val max_actual_time = graph.roads.map(_.freeflow_time).max

  private var owner: Agent = null
  override def router_type = RouterType.Tollbooth
  override def setup(a: Agent) {
    owner = a
  }

  // Score is (utility, 0)
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) => {
      // Utility = 0.1 * (1 - priority) * price + priority * time
      // Deterministically choose the first turn that fits
      val turn = prev.to.turns.find(t => t.from.road == prev && t.to.road == next).get
      val price = prev.to.intersection.tollbooth.toll(turn)   // not gonna normalize
      val time = next.freeflow_time / max_actual_time
      val priority = owner.wallet.priority
      (TollboothRouter.toll_weight * (1 - priority) * price + priority * time, 0)
    }
  )
}

class LatestEstimateRouter(graph: Graph) extends AbstractPairAstarRouter(graph) {
  private var owner: Agent = null
  override def router_type = RouterType.LatestEstimate
  override def setup(a: Agent) {
    owner = a
  }

  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (owner.sim.latest_delays.delay(next), 0)
  )
}
