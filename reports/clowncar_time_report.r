# This script is just for trip times
source("reports/common.r")
pdf("time_report.pdf")

# Definitions
colors <- c("purple", "orangered", "turquoise2", "chartreuse1", "red4", "mediumblue", "darkgreen")

mode_labels=c("Baseline", "Avoid Max", "Avoid Sum")

foreach_mode <- function(fxn) {
  modes = c("baseline", "avoid_max", "avoid_sum")
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
    axis(side=1, at=1:3, tick=FALSE, line=1, labels=mode_labels)
  }

  # Normalizing by agent is different from presenting raw individual times to begin with, because it
  # doesn't account for the grouping by scenario.
  unweighted <- dbGetQuery(db, normalize(concat(c(
    "SELECT scenario, ",
    foreach_mode(function(mode) { concat(c("SUM(", mode, ") / 60 AS ", mode)) }),
    " ", filter, " GROUP BY scenario"))))
  bplot(unweighted, "Unweighted normalized trip times", "Time per agent (minute / agent)")

  savings <- dbGetQuery(db, normalize(concat(c(
    "SELECT scenario, ",
    foreach_mode(function(mode) { concat(c("SUM(baseline - ", mode, ") / 60 AS ", mode)) }),
    " ", filter, " GROUP BY scenario"))))
  bplot(savings, "Trip time savings relative to baseline",
        "Time savings per agent (minute / agent)")
  abline(h=0)
}
