# This script is just for trip times
source("reports/common.r")
pdf("time_report.pdf")

# Definitions
colors <- c("purple", "orangered", "turquoise2", "chartreuse1", "red4", "mediumblue", "darkgreen")

mode_labels=c("FCFS\n", "Auctions\n", "Equal\n", "Fixed\n", "Auctions\n+sysbid", "Equal\n+sysbid",
              "Fixed\n+sysbid")

foreach_mode <- function(fxn) {
  modes = c("fcfs", "auctions_no_sysbids", "equal_no_sysbids", "fixed_no_sysbids",
            "auctions_sysbids", "equal_sysbids", "fixed_sysbids")
  Reduce(function(a, b) { paste(a, b, sep=", ") }, Map(fxn, modes))
}

# query must have scenario
normalize <- function(query) {
  concat(c("SELECT ", foreach_mode(function(mode) { concat(c(mode, " / count")) }),
           " FROM(", query, ") t1 INNER JOIN (", count, ") t2 ON t1.scenario == t2.scenario"))
}

if (file.exists("times.db")) {
  # During development of this script, don't recreate the DB every time
  db <- dbConnect(dbDriver("SQLite"), "times.db")
} else {
  # Read the raw data
  raw_filenames <- Sys.glob(concat(c(commandArgs(trailingOnly=TRUE)[1], "/*trip_time*")))
  raw_files <- Map(function(fn) { read.table(fn, header=TRUE) }, raw_filenames)
  raw <- do.call(rbind, raw_files)
  row.names(raw) <- seq(nrow(raw))

  # Mangle the data into SQL
  db <- dbConnect(dbDriver("SQLite"), "times.db")
  dbWriteTable(db, "raw_times", raw)
  agent_count <- dbGetQuery(
    db, "SELECT scenario, MAX(agent) AS count FROM raw_times GROUP BY scenario")
  dbWriteTable(db, "agent_count", agent_count)
}

cities <- dbGetQuery(db, "SELECT DISTINCT map FROM raw_times")
count <- "SELECT count, scenario FROM agent_count"
for (city in cities$map) {
  filter <- concat(c("FROM raw_times WHERE map='", city, "'"))
  name <- city_names[city]
  bplot <- function(data, title, ylabel) {
    boxplot2(data, col=colors, xaxt="n", ylab=ylabel, main=concat(c(title, " in ", name)))
    box()
    axis(side=1, at=1:7, tick=FALSE, line=1, labels=mode_labels)
  }

  # Normalizing by agent is different from presenting raw individual times to begin with, because it
  # doesn't account for the grouping by scenario.
  unweighted <- dbGetQuery(db, normalize(concat(c(
    "SELECT scenario, ",
    foreach_mode(function(mode) { concat(c("SUM(", mode, ") / 60 AS ", mode)) }),
    " ", filter, " GROUP BY scenario"))))
  bplot(unweighted, "Unweighted normalized trip times", "Time per agent (minute / agent)")

  weighted <- dbGetQuery(db, normalize(concat(c(
    "SELECT scenario, ",
    foreach_mode(function(mode) { concat(c("SUM(", mode, " * priority) AS ", mode)) }),
    " ", filter, " GROUP BY scenario"))))
  bplot(weighted, "Weighted normalized trip times",
        "Time * priority per agent (minute * $ / agent)")

  savings_fcfs <- dbGetQuery(db, normalize(concat(c(
    "SELECT scenario, ",
    foreach_mode(function(mode) { concat(c("SUM(fcfs - ", mode, ") / 60 AS ", mode)) }),
    " ", filter, " GROUP BY scenario"))))
  bplot(savings_fcfs, "Trip time savings relative to FCFS", "Time savings per agent (minute / agent)")
  abline(h=0)

  savings_fcfs <- dbGetQuery(db, normalize(concat(c(
    "SELECT scenario, ",
    foreach_mode(function(mode) { concat(c("SUM(equal_sysbids - ", mode, ") / 60 AS ", mode)) }),
    " ", filter, " GROUP BY scenario"))))
  bplot(savings_fcfs, "Trip time savings relative to Equal with System Bids",
        "Time savings per agent (minute / agent)")
  abline(h=0)
}
