// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

object ClownCarExperiment {
  def main(args: Array[String]) {
    new ClownCarExperiment(ExpConfig.from_args(args)).run_experiment()
  }
}

class ClownCarExperiment(config: ExpConfig) extends SmartExperiment(config, "clowncar") {
  // We never want to send somebody to a road already with more than its freeflow capacity, so the
  // max budget should be 50 (since that's the cost of a 100% freeflow-congested road).
  override def scenario_params = Array("budget=0-50")

  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new OriginalRouteMetric(info), new RoadCongestionMetric(info)
  )

  override def run() {
    val base = ScenarioPresets.transform(scenario, "clowncar_smart_intersections")

    output_data(List(
      run_trial(base, "baseline"),
      run_trial(ScenarioPresets.transform(base, "clowncar_use_dumbtoll_router"), "avoid_max"),
      run_trial(ScenarioPresets.transform(base, "clowncar_use_sumtoll_router"), "avoid_sum")
      //run_trial(ClownCarExperiment.use_router(base, RouterType.TollThreshold), "toll_threshold")
    ))
  }
}
