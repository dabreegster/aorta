require("gplots")
require("RSQLite")

city_names = list()
city_names["austin"] = "Austin"
city_names["baton_rouge"] = "Baton Rouge"
city_names["seattle"] = "Seattle"
city_names["sf"] = "San Francisco"

concat <- function(ls) {
  Reduce(function(a, b) { paste(a, b, sep="") }, ls)
}
