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

abstract class AbstractAstarRouter(graph: Graph) extends Router(graph) {
  override def path(spec: Pathfind) = AStar.path(transform(spec))

  protected def transform(spec: Pathfind) = spec
}

// straight-line distance at 1m/s for freeflow time
trait SimpleHeuristic extends AbstractAstarRouter {
  private val max_speed = Physics.mph_to_si(80.0)  // TODO put in cfg
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_heuristic = (state: Road) => state.end_pt.dist_to(spec.goal.end_pt) / max_speed
  )
  // Alternate heuristics explore MUCH less states, but the oracles are too
  // pricy. (CH, Dijkstra table of distances)
  // TODO make a fast mode that doesnt divide by speed limit. Suboptimal paths.
}

class FreeflowRouter(graph: Graph) extends AbstractAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Unusable
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: Double) => next.freeflow_time
  )
}

object TollboothRouter {
  var toll_weight = 0.1
}

class TollboothRouter(graph: Graph) extends AbstractAstarRouter(graph) {
  // TODO cache this among drivers!
  private val max_actual_time = graph.roads.map(_.freeflow_time).max

  private var owner: Agent = null
  override def router_type = RouterType.Tollbooth
  override def setup(a: Agent) {
    owner = a
  }

  // Score is (utility, 0)
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: Double) => {
      // Utility = 0.1 * (1 - priority) * price + priority * time
      // Deterministically choose the first turn that fits
      val turn = prev.to.turns.find(t => t.from.road == prev && t.to.road == next).get
      val price = prev.to.intersection.tollbooth.toll(turn)   // not gonna normalize
      val time = next.freeflow_time / max_actual_time
      val priority = owner.wallet.priority
      TollboothRouter.toll_weight * (1 - priority) * price + priority * time
    }
  )
}

class LatestEstimateRouter(graph: Graph) extends AbstractAstarRouter(graph) {
  private var owner: Agent = null
  override def router_type = RouterType.LatestEstimate
  override def setup(a: Agent) {
    owner = a
  }

  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: Double) => owner.sim.latest_delays.delay(next)
  )
}
