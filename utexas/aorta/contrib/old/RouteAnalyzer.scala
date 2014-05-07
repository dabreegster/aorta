// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

/*package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}

import utexas.aorta.map.Road
import utexas.aorta.map.analysis.{RouteFeatures, Demand}
import utexas.aorta.sim.{EV_AgentSpawned, Agent}

object RouteAnalyzer {
  def main(args: Array[String]) {
    new RouteAnalyzer(ExpConfig.from_args(args)).run()
  }
}

class RouteAnalyzer(config: ExpConfig) extends Experiment(config) {
  protected def outfn = "route-results"
  protected val output = new PrintWriter(new FileWriter(new File(outfn)))

  def run() {
    // Simulate, capturing every driver's route and trip time
    val base_sim = scenario.make_sim(graph).setup()
    val times = new TripTimeMetric(base_sim)
    val actual_paths = new RouteRecordingMetric(base_sim)
    simulate(base_sim)

    // Simulate again, scoring the path that the agent is destined to take at the time they spawn
    io.notify("Round 0 done, precomputing demand on roads/intersections")
    val demand = Demand.demand_for(scenario, graph)

    val sim_again = scenario.make_sim(graph).setup()
    sim_again.listen("route-analyzer", _ match {
      case EV_AgentSpawned(a) => {
        val score = score_path(actual_paths(a.id), demand)
        output_score(a, score, times(a.id))
      }
      case _ =>
    })
    simulate(sim_again)
    output.close()

    config.gs_prefix match {
      case Some(prefix) => Runtime.getRuntime.exec(Array(
        "gsutil", "cp", outfn, prefix + "results_" + graph.basename
      ))
      case None =>
    }
  }

  protected def output_score(a: Agent, score: RouteFeatures, trip_time: Double) {
    output.println((score.toList ++ List(scenario.agents.size, trip_time)).mkString(","))
  }

  private def score_path(path: List[Road], demand: Demand) =
    path
      .map(step => RouteFeatures.for_step(step, demand))
      .fold(RouteFeatures.BLANK)((a, b) => a + b)
}*/

/*
class RouteRecordingMetric(info: MetricInfo) extends Metric(info) {
  override def name = "route_recording"

  private val routes = new mutable.HashMap[AgentID, mutable.ListBuffer[Road]]()

  info.sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, from, to: Turn) => {
      val path = routes.getOrElseUpdate(a.id, new mutable.ListBuffer[Road]())
      if (path.isEmpty) {
        path += to.from.road
      }
      path += to.to.road
    }
    case _ =>
  })

  def apply(a: AgentID) = routes(a).toList
  override def output(ls: List[Metric]) {
    throw new UnsupportedOperationException("Why save the actual routes?")
  }
}
*/
