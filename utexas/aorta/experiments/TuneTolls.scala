// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.map.TollboothRouter

import scala.collection.mutable

// A good test: ./experiment maps/baton_rouge.map --spawn 5000 delay=300 --preset milo_gps
object TuneTolls {
  def main(args: Array[String]) {
    new TuneTolls(ExpConfig.from_args(args)).run_experiment()
  }
}

class TuneTolls(config: ExpConfig) extends SmartExperiment(config, "tune_tolls") {
  val toll_weights = List(0.1, 0.01, 0.001, 0.0001, 0.00001)

  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new TripDistanceMetric(info), new TripPathsMetric(info)
  )

  override def run() {
    val results = new mutable.ListBuffer[List[Metric]]()
    results += run_trial(ScenarioPresets.transform(scenario, "milo_gps"), "baseline")

    for (weight <- toll_weights) {
      val base = ScenarioPresets.transform(scenario, "milo_milo")
      val s = base.copy(sim_config = base.sim_config.copy(tollbooth_weight = weight))
      results += run_trial(s, s"tolls_$weight")
    }

    output_data(results.toList)
  }
}
