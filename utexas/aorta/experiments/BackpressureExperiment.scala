// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

object BackpressureExperiment {
  def main(args: Array[String]) {
    new BackpressureExperiment(ExpConfig.from_args(args)).run_experiment()
  }
}

class BackpressureExperiment(config: ExpConfig) extends SmartExperiment(config, "backpressure") {
  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new TurnDelayMetric(info)
  )

  override def run() {
    val fcfs = scenario
    val backpressure = ScenarioPresets.transform(fcfs, "backpressure_use_backpressure")

    output_data(List(
      run_trial(fcfs, "fcfs_mixed"),
      run_trial(backpressure, "backpressure_mixed"),
      run_trial(ScenarioPresets.transform(fcfs, "backpressure_use_reservation"), "fcfs_reservation"),
      run_trial(ScenarioPresets.transform(backpressure, "backpressure_use_reservation"),
                "backpressure_reservation")
    ))
  }
}
