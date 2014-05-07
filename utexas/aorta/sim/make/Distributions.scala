// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.make

import utexas.aorta.map.{Graph, Road}
import utexas.aorta.common.{RNG, cfg,  AgentID, Poisson}

import scala.collection.mutable

object IntersectionDistribution {
  private val rng = new RNG()

  val all_policies = IntersectionType.values.toArray
  val all_orderings = OrderingType.values.toArray
  lazy val default_policy = IntersectionType.withName(cfg.policy)
  lazy val default_ordering = OrderingType.withName(cfg.ordering)

  // TODO specify "80% x, 20% y" for stuff...
  def uniform(graph: Graph, policies: Array[IntersectionType.Value],
              orderings: Array[OrderingType.Value]) =
    graph.vertices.map(v => MkIntersection(
      v.id, rng.choose(policies), rng.choose(orderings)
    ))

  def uniform_default(graph: Graph) = uniform(
    graph, Array(default_policy), Array(default_ordering)
  )

  def default(graph: Graph) = realistic(graph)

  // Put stop signs at crossings of all small roads, signals at
  // crossings of all big roads, and common case hybrids at mixtures
  def realistic(graph: Graph) = graph.vertices.map(v => {
    val (big, small) = v.roads.partition(_.is_major)
    val policy =
      if (big.isEmpty)
        IntersectionType.StopSign
      else if (small.isEmpty)
        IntersectionType.Signal
      else
        IntersectionType.Yield
    MkIntersection(v.id, policy, default_ordering)
  })
}

object AgentDistribution {
  // TODO share RNGs so the cmdline tool can be parametrized by one random
  // thing?
  private val rng = new RNG()

  val all_wallets = WalletType.values.toArray
  lazy val default_wallet = WalletType.withName(cfg.wallet)

  def filter_candidates(starts: Array[Road]) = starts.filter(_.rightmost.ok_to_spawn)

  // TODO specify "80% x, 20% y" for stuff...
  def uniform_times(ids: Range, starts: Array[Road], ends: Array[Road],
              times: (Double, Double),
              wallets: Array[WalletType.Value],
              budgets: (Int, Int)): Array[MkAgent] =
  {
    val actual_starts = filter_candidates(starts)
    return ids.map(id => {
      val start = rng.choose(actual_starts)
      val budget = rng.int(budgets._1, budgets._2)
      val raw_time = rng.double(times._1, times._2)
      val time = raw_time - (raw_time % cfg.dt_s) // TODO may still have fp issue
      MkAgent(
        new AgentID(id), time, start.id, start.rightmost.safe_spawn_dist(rng),
        MkRoute(
          RouterType.Congestion, RouterType.Congestion, Array(), rng.choose(ends).id,
          ReroutePolicyType.Never
        ),
        MkWallet(rng.choose(wallets), budget, rng.double(0, 1), false /* bid_ahead */)
      )
    }).toArray
  }

  // TODO unify with uniform_times, and figure out how to split up factors a little.
  def poisson_times(
    first_id: AgentID, starts: Array[Road], ends: Array[Road], start_time: Double,
    end_time: Double, vehicles_per_hour: Int, wallets: Array[WalletType.Value], budgets: (Int, Int)
  ): Array[MkAgent] = {
    val actual_starts = filter_candidates(starts)
    val n = (vehicles_per_hour * ((end_time - start_time) / 3600.0)).toInt
    var id = first_id.int
    val result = new mutable.ArrayBuffer[MkAgent]()
    for (raw_spawn_time <- new Poisson(rng, n, start_time, end_time)) {
      val start = rng.choose(actual_starts)
      val budget = rng.int(budgets._1, budgets._2)
      val time = raw_spawn_time - (raw_spawn_time % cfg.dt_s) // TODO may still have fp issue
      id += 1
      result += MkAgent(
        new AgentID(id), time, start.id, start.rightmost.safe_spawn_dist(rng),
        MkRoute(
          RouterType.Congestion, RouterType.Congestion, Array(), rng.choose(ends).id,
          ReroutePolicyType.Never
        ),
        MkWallet(rng.choose(wallets), budget, rng.double(0, 1), false /* bid_ahead */)
      )
    }
    return result.toArray
  }

  def default(graph: Graph) = uniform_times(
    Range(0, cfg.army_size), graph.roads, graph.roads, (0.0, 60.0), Array(default_wallet), (100, 200)
  )
}
