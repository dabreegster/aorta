pdf("sim_speed.pdf")

colors <- c("purple", "orangered", "turquoise2", "chartreuse1", "red4", "mediumblue", "darkgreen")

files <- commandArgs(trailingOnly=TRUE)
plot(read.table(files[1], header=TRUE), type="n", main="Sim speed comparison") # Don't plot yet
legend(x="top", files, fill=colors)
i <- 1
for (fn in files) {
  lines(read.table(fn, header=TRUE), col=colors[i])
  i <- i + 1
}
