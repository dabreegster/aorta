# This script is just for road congestion
source("reports/common.r")
pdf("congestion_report.pdf")

# Definitions
color1 <- "blue"
color2 <- "red"
color3 <- "green"

if (file.exists("congestion.db")) {
  # During development of this script, don't recreate the DB every time
  db <- dbConnect(dbDriver("SQLite"), "congestion.db")
} else {
  # Read the raw data
  raw_filenames <- Sys.glob(concat(c(commandArgs(trailingOnly=TRUE)[1], "/*road_congestion*")))
  raw_files <- Map(function(fn) { read.table(fn, header=TRUE) }, raw_filenames)
  raw <- do.call(rbind, raw_files)
  row.names(raw) <- seq(nrow(raw))

  # Mangle the data into SQL
  db <- dbConnect(dbDriver("SQLite"), "congestion.db")
  dbWriteTable(db, "congestion", raw)
}

cities <- dbGetQuery(db, "SELECT DISTINCT map FROM congestion")
for (city in cities$map) {
  grab <- function(mode) {
    slice = dbGetQuery(db, concat(c(
      "SELECT road_congestion_bin, count FROM congestion WHERE MAP='", city, "' AND MODE='", mode,
      "'")))
    rep(slice$road_congestion_bin, slice$count)
  }
  name <- city_names[city]

  hist(grab("baseline"), col=color1, main=concat(c("Road congestion in ", name)),
       xlab="Percent of freeflow capacity filled", density=5, angle=180)
  hist(grab("avoid_max"), col=color2, add=TRUE, density=5, angle=45)
  hist(grab("avoid_sum"), col=color3, add=TRUE, density=5, angle=135, border="black")
  legend(x="top", c("Baseline", "Avoid Max", "Avoid Sum"), fill=c(color1, color2, color3))

  # Only the data >100%
  onlybig <- function(data) {
    subset(data, data > 100)
  }
  hist(onlybig(grab("baseline")), col=color1,
       main=concat(c("Road congestion in ", name, " (>100%)")),
       xlab="Percent of freeflow capacity filled", density=5, angle=180)
  hist(onlybig(grab("avoid_max")), col=color2, add=TRUE, density=5, angle=45)
  hist(onlybig(grab("avoid_sum")), col=color3, add=TRUE, density=5, angle=135, border="black")
  legend(x="top", c("Baseline", "Avoid Max", "Avoid Sum"), fill=c(color1, color2, color3))

  # TODO and under 100 only...
}
