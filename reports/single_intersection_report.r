require(ggplot2)
raw <- read.table("results_r.dat", header=TRUE)
attach(raw)

cor(unweighted_time_auctions, weighted_time_auctions)

qplot(vehicles_per_hour, unweighted_time_baseline - unweighted_time_auctions, color =
      use_sysbids, xlab = "Vehicles per hour", ylab = "Auction time savings relative to baseline",
      main = "Effect of system bids on trip time savings")

qplot(vehicles_per_hour, unweighted_time_baseline - unweighted_time_auctions,
      data=subset(raw, use_sysbids == TRUE), colour = bid_ahead,
      main = "Trip time savings with system bids")
