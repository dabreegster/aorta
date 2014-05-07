# This script is just for trip times
source("reports/common.r")
pdf("money_report.pdf")
dbfn <- "auction_money.db"

# TODO refactor this pattern
if (file.exists(dbfn)) {
  # During development of this script, don't recreate the DB every time
  db <- dbConnect(dbDriver("SQLite"), dbfn)
} else {
  # Read the raw data. TODO refactor this bit
  raw_filenames <- Sys.glob(concat(c(commandArgs(trailingOnly=TRUE)[1], "/*money_spent*")))
  raw_files <- Map(function(fn) { read.table(fn, header=TRUE) }, raw_filenames)
  raw <- do.call(rbind, raw_files)
  row.names(raw) <- seq(nrow(raw))

  # Mangle the data into SQL
  db <- dbConnect(dbDriver("SQLite"), dbfn)
  dbWriteTable(db, "money", raw)
}

color1 <- "red"
color2 <- rgb(0, 0, 1, 0.25)

# Don't group by city, scenario, anything.
raw <- dbGetQuery(db, "SELECT auctions_sysbids, auctions_no_sysbids, priority FROM money")
attach(raw)
hist(auctions_sysbids, col=color1, main="Histogram of total money spent per agent",
     xlab="Money spent (cents)")
hist(auctions_no_sysbids, col=color2, add=TRUE)
legend(x="top", c("Auctions with system bids", "Auctions without system bids"),
       fill=c(color1, color2))

hist(100 * auctions_sysbids / priority, col=color1,
     main="Histogram of percent of total budget spent per agent", xlab="Percent of budget spent")
hist(100 * auctions_no_sysbids / priority, col=color2, add=TRUE)
legend(x="top", c("Auctions with system bids", "Auctions without system bids"),
       fill=c(color1, color2))
