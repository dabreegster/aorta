// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.map.{Graph, Road, AbstractPairAstarRouter, SimpleHeuristic, Turn, Edge}
import utexas.aorta.sim.{EV_AgentSpawned, EV_Transition}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.{Scenario, RouterType}
import utexas.aorta.common.{Util, RNG}
import utexas.aorta.common.algorithms.Pathfind

import scala.collection.mutable

object DTAExperiment {
  def main(args: Array[String]) {
    new DTAExperiment(ExpConfig.from_args(args)).run_experiment()
  }
}

// Dynamic traffic assignment
class DTAExperiment(config: ExpConfig) extends SmartExperiment(config, "dta") {
  private val iterations = 5  // TODO put in ExpConfig
  private val rng = new RNG()

  // TODO indicate which agents had routes shifted in each round.
  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new OriginalRouteMetric(info), new LinkDelayMetric(info)
  )

  override def run() {
    val results = new mutable.ListBuffer[List[Metric]]()

    var current_scenario = scenario
    for (round <- Range(0, iterations)) {
      val metrics = run_trial(current_scenario, s"dta_$round")
      results += metrics
      if (round != iterations - 1) {
        val delay = metrics.last.asInstanceOf[LinkDelayMetric]  // TODO bit of a hack.
        // Min round value is 2
        current_scenario = change_paths(current_scenario, delay, 1.0 / (round + 2))
      }
    }

    output_data(results.toList)
  }

  // Reroute some drivers using actual delays
  private def change_paths(
    base_scenario: Scenario, delay: LinkDelayMetric, percent: Double
  ): Scenario = {
    val graph = base_scenario.graph
    return base_scenario.copy(agents = base_scenario.agents.map(a => {
      // TODO choose lucky part of population based on budget?
      if (rng.percent(percent)) {
        // Replan!
        // TODO spawn vs start time...
        val new_path = new TimeDependentAStar(graph, delay, a.birth_tick)
          .path(graph.get_r(a.start), graph.get_r(a.route.goal)).path
          .map(_.id)
          .toArray
        a.copy(route = a.route.copy(orig_router = RouterType.Fixed, initial_path = new_path))
        // TODO make these delays available to all/some drivers, for rerouting? could introduce bad
        // biases towards regions that should be clear but arent, though.
      } else {
        a
      }
    }))
  }
}

class TimeDependentAStar(graph: Graph, delays: LinkDelayMetric, start_time: Double)
  extends AbstractPairAstarRouter(graph) with SimpleHeuristic
{
  override def router_type = RouterType.Unusable
  override def transform(spec: Pathfind) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      // cost_sofar._2 is the time spent in the route so far
      (Util.bool2binary(next.road_agent.congested), delays.delay(next, start_time + cost_sofar._2))
  )
}

class LinkDelayMetric(info: MetricInfo) extends Metric(info) {
  override def name = "link_delay"

  // TODO will this eat too much memory?
  private val delays_per_time = info.sim.graph.roads.map(
    r => r -> new java.util.TreeMap[Double, Double]()
  ).toMap
  private val entry_time = new mutable.HashMap[Agent, Double]()

  info.sim.listen(classOf[EV_Transition], _ match {
    // Entering a road
    case EV_Transition(a, from: Turn, to) => entry_time(a) = a.sim.tick
    // Exiting a road that we didn't spawn on
    case EV_Transition(a, from: Edge, to: Turn) if entry_time.contains(a) =>
      add_delay(entry_time(a), a.sim.tick - entry_time(a), from.road)
    case _ =>
  })

  private def add_delay(entry_time: Double, delay: Double, at: Road) {
    // Two agents can enter the same Road at the same time (on different lanes)
    // Just arbitrarily overwrite if there's a conflict
    delays_per_time(at).put(entry_time, delay)
  }

  override def output(ls: List[Metric]) {
    // Don't actually save anything!
  }

  // Many possible interpolations for this...
  def delay(on: Road, at: Double) = delays_per_time(on).lowerKey(at) match {
    // 'at' is before all entries here? then the road's clear
    case 0.0 => on.freeflow_time  // TODO 0.0 is how failure gets encoded by java treemap...
    case entry_time => delays_per_time(on).get(entry_time) match {
      // 'at' happens after the most recent entry finishes
      case delay if at > entry_time + delay => on.freeflow_time
      // This instance overlaps 'at', so just use the same delay.
      case delay => delay
    }
  }
}
