// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.map.{CongestionRouter}

import utexas.aorta.common.{Util, RNG, Timer, cfg}

object BenchmarkRouting {
  def main(args: Array[String]) {
    val rounds = 10000
    val print_every = 100

    val sim = Util.process_args(args)
    val rng = new RNG()

    val routers = List(
      (new CongestionRouter(sim.graph), "congestion_a*")
    )
    val sum_times = Array.fill[Double](routers.size)(0.0)
    val sum_nodes = Array.fill[Int](routers.size)(0)

    for (i <- 1 until rounds) {
      val from = rng.choose(sim.graph.roads)
      val to = rng.choose(sim.graph.roads)

      for (((router, name), idx) <- routers.zipWithIndex) {
        val t = Timer(name)
        val nodes = router.path(from, to).nodes_visited
        sum_times(idx) += t.so_far
        sum_nodes(idx) += nodes
      }

      if (i % print_every == 0) {
        Util.log(f"round $i%,d / $rounds%,d")
        for (((_, name), idx) <- routers.zipWithIndex) {
          Util.log(s"  $name: ${sum_times(idx)}s total, ${sum_times(idx) / i}s per path")
          Util.log("  %s: %,d nodes per path".format(name, sum_nodes(idx) / i))
        }
      }
    }
  }
}
