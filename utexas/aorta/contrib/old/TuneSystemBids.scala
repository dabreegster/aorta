// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

/*package utexas.aorta.analysis

import scala.collection.mutable

import utexas.aorta.sim.{Scenario, SystemWalletConfig}
import utexas.aorta.map.Graph

import utexas.aorta.common.{Util, Common, cfg, Flags}

// TODO synthetic is a bad map to use, since it neglects ready_bonus

// Use hillclimbing to tune the rates of system bids.
object TuneSystemBids {
  // (thruput_bonus, capacity_bonus, dependency_rate, waiting_rate, ready_bonus)
  type State = (Int, Int, Int, Int, Int)

  // Stop after this long or when we can't do any better
  val iters = 100

  // Cut off after how many hours?
  val run_for = 1 * 3600

  var graph: Graph = null
  var base: Scenario = null

  // Feed it the scenario to use as a metric
  def main(args: Array[String]): Unit = {
    Flags.set("--savestate", "false")
    Flags.set("--replay", "false")
    base = Scenario.load(args.head)
    graph = Graph.load(base.map_fn)

    // The parameters of the SystemWalletConfig
    // (Leave avail_capacity_threshold fixed)
    var current = (1, 1, 1, 1, 1)
    var iter = 1
    var continue = true
    while (continue) {
      println(s"Iteration $iter: base $current with score ${score(current)}")
      val next = generate_neighbors(current).maxBy(state => score(state))

      iter += 1
      if (iter == iters || score(next) <= score(current)) {
        continue = false
      } else {
        current = next
      }
    }

    println(s"\nBest is $current with ${score(current)}")
  }

  private def generate_neighbors(state: State) =
    state.copy(_1 = bound(state._1 - 1)) :: state.copy(_1 = bound(state._1 + 1)) ::
    state.copy(_2 = bound(state._2 - 1)) :: state.copy(_2 = bound(state._2 + 1)) ::
    state.copy(_3 = bound(state._3 - 1)) :: state.copy(_3 = bound(state._3 + 1)) ::
    state.copy(_4 = bound(state._4 - 1)) :: state.copy(_4 = bound(state._4 + 1)) ::
    state.copy(_5 = bound(state._5 - 1)) :: state.copy(_5 = bound(state._5 + 1)) ::
    Nil
  private def bound(i: Int) = math.max(1, math.min(10, i))

  val scores = new mutable.HashMap[State, Int]()
  private def score(state: State): Int = {
    if (scores.contains(state)) {
      return scores(state)
    } else {
      val mod = base.copy(system_wallet = SystemWalletConfig(
        thruput_bonus = state._1,
        capacity_bonus = state._2,
        dependency_rate = state._3,
        waiting_rate = state._4,
        ready_bonus = state._5
      ))
      val sim = mod.make_sim(graph)
      sim.setup()

      // Run the simulation until its cut-off and rank it.
      var finished = 0
      try {
        println(s"  Testing $state")
        var last_time = System.currentTimeMillis
        while (sim.done == false && sim.tick < run_for) {
          sim.step()
          val now = System.currentTimeMillis
          if (now - last_time > 1000 * 1) {
            print(s"\r    It's ${Util.time_num(sim.tick)} and ${sim.finished_count} have finished")
            last_time = now
          }
        }

        // The more agents that completed, the better.
        finished = sim.finished_count
      } catch {
        case e: Throwable => {
          println(s"  !!! Simulation of $state broke... $e")
        }
      }

      println(s"\n  $state had $finished agents finish")
      scores(state) = finished
      return finished
    }
  }
}*/
