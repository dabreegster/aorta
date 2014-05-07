// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

object MILOExperiment {
  def main(args: Array[String]) {
    new MILOExperiment(ExpConfig.from_args(args)).run_experiment()
  }
}

class MILOExperiment(config: ExpConfig) extends SmartExperiment(config, "milo") {
  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new TripDistanceMetric(info), new TripPathsMetric(info),
    new RoadUsageMetric(info)
  )

  override def run() {
    output_data(List(
      //run_trial(ScenarioPresets.transform(scenario, "milo_today"), "today"),
      run_trial(ScenarioPresets.transform(scenario, "milo_gps"), "gps"),
      run_trial(ScenarioPresets.transform(scenario, "milo_milo"), "milo")
    ))
  }
}
