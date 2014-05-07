// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

// TODO get a macro or something for main, or have a more flexible cmdline tool
// likewise for the scripts
object AuctionExperiment {
  def main(args: Array[String]) {
    new AuctionExperiment(ExpConfig.from_args(args)).run_experiment()
  }
}

class AuctionExperiment(config: ExpConfig) extends SmartExperiment(config, "auction") {
  override def scenario_params = Array("budget=0-500")

  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new OriginalRouteMetric(info), new MoneySpentMetric(info),
    new TurnDelayMetric(info), new TurnCompetitionMetric(info)
  )

  override def run() {
    val fcfs = ScenarioPresets.transform(scenario, "auctions_enable_bidahead")
    val sysbid_base = ScenarioPresets.transform(fcfs, "auctions_enable_auctions")
    val nosys_base = ScenarioPresets.transform(sysbid_base, "auctions_disable_sysbids")

    output_data(List(
      run_trial(fcfs, "fcfs"),
      run_trial(sysbid_base, "auctions_sysbids"),
      run_trial(nosys_base, "auctions_no_sysbids"),
      run_trial(ScenarioPresets.transform(sysbid_base, "auctions_equal_budgets"), "equal_sysbids"),
      run_trial(ScenarioPresets.transform(nosys_base, "auctions_equal_budgets"), "equal_no_sysbids"),
      run_trial(ScenarioPresets.transform(sysbid_base, "auctions_fixed_budgets"), "fixed_sysbids"),
      run_trial(ScenarioPresets.transform(nosys_base, "auctions_fixed_budgets"), "fixed_no_sysbids")
    ))
  }
}
