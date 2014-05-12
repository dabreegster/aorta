// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.sim.make.{Scenario, OrderingType, IntersectionType, RouterType,
                              ReroutePolicyType, WalletType, SystemWalletConfig}

// Transform scenarios for various experiments. It's a bit odd to group these in one place, but it's
// convenient for invoking the same transformations from the command-line.
object ScenarioPresets {
  def transform(s: Scenario, mode: String) = mode match {
    // TODO shouldnt be enable/disable and small mods, should be full standalone spec. name like
    // the mode in each experiment.

    case "auction_enable_bidahead" => s.copy(
      agents = s.agents.map(a => a.copy(wallet = a.wallet.copy(bid_ahead = true)))
    )
    case "auctions_enable_auctions" => s.copy(
      intersections = s.intersections.map(_.copy(ordering = OrderingType.Auction))
    )
    case "auctions_disable_sysbids" => s.copy(sim_config = s.sim_config.copy(
      system_wallet = SystemWalletConfig.blank
    ))
    case "auctions_equal_budgets" => s.copy(
      agents = s.agents.map(a => a.copy(wallet = a.wallet.copy(budget = 1, policy = WalletType.Static)))
    )
    case "auctions_fixed_budgets" => s.copy(
      agents = s.agents.map(a => a.copy(wallet = a.wallet.copy(policy = WalletType.Static)))
    )

    case "backpressure_use_backpressure" => s.copy(
      intersections = s.intersections.map(_.copy(ordering = OrderingType.Pressure))
    )
    case "backpresure_use_reservation" => s.copy(
      intersections = s.intersections.map(_.copy(policy = IntersectionType.Reservation))
    )

    // TODO there are many baselines
    case "milo_today" => s.copy(
      intersections = s.intersections.map(_.copy(policy = IntersectionType.Signal)),
      agents = s.agents.map(a => a.copy(route = a.route.copy(
        orig_router = RouterType.Freeflow, rerouter = RouterType.Freeflow,
        reroute_policy = ReroutePolicyType.Never
      )))
    )
    case "milo_gps" => s.copy(
      intersections = s.intersections.map(_.copy(policy = IntersectionType.Signal)),
      agents = s.agents.map(a => a.copy(route = a.route.copy(
        orig_router = RouterType.LatestEstimate, rerouter = RouterType.LatestEstimate,
        reroute_policy = ReroutePolicyType.Never//Regularly
      )))
    )
    case "milo_milo" => s.copy(
      intersections = s.intersections.map(_.copy(policy = IntersectionType.Signal)),
      agents = s.agents.map(a => a.copy(route = a.route.copy(
        orig_router = RouterType.Tollbooth, rerouter = RouterType.Tollbooth,
        reroute_policy = ReroutePolicyType.Never//Regularly
      )))
    )

    case _ => throw new IllegalArgumentException(s"Mode $mode doesn't exist")
  }
}
