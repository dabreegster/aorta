// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.make

import utexas.aorta.map._
import utexas.aorta.sim._
import utexas.aorta.sim.drivers._
import utexas.aorta.sim.intersections._

import Function.tupled
import scala.collection.mutable

import utexas.aorta.common.{Util, StateWriter, StateReader, AgentID, VertexID, RoadID, cfg,
                            Serializable}

// Array index and agent/intersection ID must correspond. Creator's responsibility.
case class Scenario(
  name: String, map_fn: String, agents: Array[MkAgent], intersections: Array[MkIntersection],
  system_wallet: SystemWalletConfig
) extends Serializable {
  def graph = Graph.load(map_fn)
  def make_sim() = new Simulation(this)
  def save() {
    val w = Util.writer(name)
    serialize(w)
    w.done()
  }

  def make_intersection(v: Vertex, sim: Simulation) = intersections(v.id.int).make(v, sim)

  // Although the massive numbers of agents were probably created with a
  // distribution function originally, we just see the individual list in the
  // end. So compute basic stats about it.
  def summarize() {
    Util.log(s"Scenario $name for $map_fn\n")
    // TODO breakdown combos of policy/ordering, and wallet/budget
    Util.log("Intersection policies:")
    ScenarioUtil.percentages(intersections.map(_.policy))
    Util.log("Intersection orderings:")
    ScenarioUtil.percentages(intersections.map(_.ordering))
    Util.log("")

    Util.log(s"${agents.size} agents total")
    // TODO where are agents starting/going?
    if (agents.nonEmpty) {
      Util.log_push
      Util.log("Spawning time (s): " + ScenarioUtil.basic_stats(agents.map(_.birth_tick)))
      Util.log("Orig routers:")
      ScenarioUtil.percentages(agents.map(_.route.orig_router))
      Util.log("Rerouters:")
      ScenarioUtil.percentages(agents.map(_.route.rerouter))
      Util.log("Wallets:")
      ScenarioUtil.percentages(agents.map(_.wallet.policy))
      Util.log("Budget ($): " + ScenarioUtil.basic_stats_int(agents.map(_.wallet.budget)))
      Util.log("Priority: " + ScenarioUtil.basic_stats(agents.map(_.wallet.priority)))
      Util.log_pop
    }

    Util.log("System wallet rates:")
    Util.log(system_wallet.toString)
  }

  def diff(other: Scenario): Unit = {
    if (map_fn != other.map_fn) {
      Util.log(s"Scenarios are for different maps: $map_fn and ${other.map_fn}")
      return
    }
    intersections.zip(other.intersections).foreach(tupled((i1, i2) => i1.diff(i2)))
    agents.zip(other.agents).foreach(tupled((a1, a2) => a1.diff(a2)))
    if (agents.size != other.agents.size) {
      Util.log(s"Scenarios have different numbers of agents: ${agents.size} and ${other.agents.size}")
    }

    // TODO diff SystemWalletConfig
  }

  def serialize(w: StateWriter) {
    w.strings(name, map_fn)
    w.lists(agents, intersections)
    w.obj(system_wallet)
  }
}

object Scenario {
  def unserialize(r: StateReader) = Scenario(
    r.string, r.string,
    Range(0, r.int).map(_ => MkAgent.unserialize(r)).toArray,
    Range(0, r.int).map(_ => MkIntersection.unserialize(r)).toArray,
    SystemWalletConfig.unserialize(r)
  )

  def load(fn: String) = unserialize(Util.reader(fn))

  def default(map_fn: String): Scenario = {
    val graph = Graph.load(map_fn)
    val s = Scenario(
      s"scenarios/default_${graph.name}",
      map_fn,
      AgentDistribution.default(graph),
      IntersectionDistribution.default(graph),
      SystemWalletConfig()
    )
    // Always save it, so resimulation is easy.
    Util.mkdir("scenarios")
    s.save()
    return s
  }
}

// The "Mk" prefix means "Make". These're small serializable classes to make
// agents/intersections/etc.

case class MkAgent(id: AgentID, birth_tick: Double, start: RoadID, start_dist: Double,
                   route: MkRoute, wallet: MkWallet) extends Ordered[MkAgent] with Serializable
{
  // break ties by ID
  def compare(other: MkAgent) = implicitly[Ordering[Tuple2[Double, Integer]]].compare(
    (other.birth_tick, other.id.int), (birth_tick, id.int)
  )

  def make(sim: Simulation) = new Agent(id, route.make(sim), wallet.make, sim)

  def serialize(w: StateWriter) {
    w.int(id.int)
    w.double(birth_tick)
    w.int(start.int)
    w.double(start_dist)
    w.objs(route, wallet)
  }

  def diff(other: MkAgent) {
    Util.assert_eq(id, other.id)
    val d = List(
      ScenarioUtil.diff(birth_tick, other.birth_tick, "spawn time"),
      ScenarioUtil.diff(start, other.start, "start"),
      ScenarioUtil.diff(start_dist, other.start_dist, "start distance"),
      ScenarioUtil.diff(route.goal, other.route.goal, "end"),
      ScenarioUtil.diff(route.orig_router, other.route.orig_router, "orig_router"),
      ScenarioUtil.diff(route.rerouter, other.route.rerouter, "rerouter"),
      ScenarioUtil.diff(wallet.policy, other.wallet.policy, "wallet"),
      ScenarioUtil.diff(wallet.budget, other.wallet.budget, "budget"),
      ScenarioUtil.diff(wallet.priority, other.wallet.priority, "priority")
    ).flatten.mkString(", ")
    if (d.nonEmpty) {
      Util.log(s"Agent $id different: $d")
    }
  }

  // TODO belongs elsewhere?
  // Excludes start road, since driver could start towards the end of the first road, and ideal_path
  // must be the quickest route (even if it's impossible to achieve by a few seconds)
  def ideal_path(graph: Graph) =
    new FreeflowRouter(graph).path(graph.get_r(start), graph.get_r(route.goal)).path.tail
  // TODO doesnt account for turns
  // Since we exclude the first road, it's possible to end up with a 0-length path and 0
  // time/distance
  def ideal_time(graph: Graph) = math.max(cfg.epsilon, ideal_path(graph).map(_.freeflow_time).sum)
  def ideal_distance(graph: Graph) = math.max(cfg.epsilon, ideal_path(graph).map(_.length).sum)
}

object MkAgent {
  def unserialize(r: StateReader) = MkAgent(
    new AgentID(r.int), r.double, new RoadID(r.int), r.double, MkRoute.unserialize(r),
    MkWallet.unserialize(r)
  )
}

case class MkRoute(
  orig_router: RouterType.Value, rerouter: RouterType.Value, initial_path: Array[RoadID],
  goal: RoadID, reroute_policy: ReroutePolicyType.Value
) extends Serializable {
  def make(sim: Simulation) = new PathRoute(
    sim.graph.get_r(goal),
    RouterType.make(orig_router, sim.graph, initial_path.map(id => sim.graph.get_r(id)).toList),
    RouterType.make(rerouter, sim.graph, Nil), reroute_policy
  )

  def serialize(w: StateWriter) {
    w.ints(orig_router.id, rerouter.id)
    w.list_int(initial_path.map(_.int))
    w.ints(goal.int, reroute_policy.id)
  }
}

object MkRoute {
  def unserialize(r: StateReader) = MkRoute(
    RouterType(r.int), RouterType(r.int),
    Range(0, r.int).map(_ => new RoadID(r.int)).toArray, new RoadID(r.int), ReroutePolicyType(r.int)
  )
}

// priority is [0, 1]
case class MkWallet(policy: WalletType.Value, budget: Int, priority: Double, bid_ahead: Boolean)
  extends Serializable
{
  def make() = WalletType.make(policy, budget, priority, bid_ahead)

  def serialize(w: StateWriter) {
    w.ints(policy.id, budget)
    w.double(priority)
    w.bool(bid_ahead)
  }
}

object MkWallet {
  def unserialize(r: StateReader) = MkWallet(WalletType(r.int), r.int, r.double, r.bool)
}

case class MkIntersection(id: VertexID, policy: IntersectionType.Value,
                          ordering: OrderingType.Value) extends Serializable
{
  def make(v: Vertex, sim: Simulation) = new Intersection(
    v, IntersectionType.make(v, policy, ordering, sim), sim
  )

  def diff(other: MkIntersection) {
    Util.assert_eq(id, other.id)
    val d = List(
      ScenarioUtil.diff(policy, other.policy, "policy"),
      ScenarioUtil.diff(ordering, other.ordering, "ordering")
    ).flatten.mkString(", ")
    if (d.nonEmpty) {
      Util.log(s"Intersection $id different: $d")
    }
  }

  def serialize(w: StateWriter) {
    w.ints(id.int, policy.id, ordering.id)
  }
}

object MkIntersection {
  def unserialize(r: StateReader) = MkIntersection(
    new VertexID(r.int), IntersectionType(r.int), OrderingType(r.int)
  )
}

case class SystemWalletConfig(
  thruput_bonus: Int            = 7,
  avail_capacity_threshold: Int = 25,
  capacity_bonus: Int           = 5,
  // This one easily dominates decisions
  dependency_rate: Int          = 2,
  waiting_rate: Int             = 1,
  ready_bonus: Int              = 5
) extends Serializable {
  override def toString =
    s"SystemWalletConfig(thruput_bonus = $thruput_bonus, avail_capacity_threshold = $avail_capacity_threshold, capacity_bonus = $capacity_bonus, dependency_rate = $dependency_rate, waiting_rate = $waiting_rate, ready_bonus = $ready_bonus)"

  def serialize(w: StateWriter) {
    w.ints(
      thruput_bonus, avail_capacity_threshold, capacity_bonus, dependency_rate, waiting_rate,
      ready_bonus
    )
  }
}

object SystemWalletConfig {
  def unserialize(r: StateReader) = SystemWalletConfig(r.int, r.int, r.int, r.int, r.int, r.int)
  def blank = SystemWalletConfig(0, 0, 0, 0, 0, 0)
}

// Enumeration stuff

object IntersectionType extends Enumeration {
  type IntersectionType = Value
  val NeverGo, StopSign, Signal, Reservation, Yield, AIM, Batch = Value

  def make(v: Vertex, policy: IntersectionType.Value, ordering: OrderingType.Value,
           sim: Simulation) = policy match
  {
    case NeverGo => new NeverGoPolicy(v)
    case StopSign => new StopSignPolicy(v, OrderingType.make[Ticket](ordering))
    case Signal => new SignalPolicy(v, OrderingType.make[Phase](ordering))
    case Reservation => new ReservationPolicy(v, OrderingType.make[Ticket](ordering))
    case Yield => new YieldPolicy(v, OrderingType.make[Ticket](ordering))
    case AIM => new AIMPolicy(v, OrderingType.make[Ticket](ordering))
    case Batch => new BatchPolicy(v)
  }
}

object RouterType extends Enumeration {
  type RouterType = Value
  // Agents don't use Unusable; it's just for manually-invoked routers.
  val Unusable, Freeflow, Fixed, Tollbooth, LatestEstimate = Value

  def make(enum: RouterType.Value, graph: Graph, initial_path: List[Road]) = enum match {
    case Freeflow => new FreeflowRouter(graph)
    case Fixed => new FixedRouter(graph, initial_path)
    case Tollbooth => new TollboothRouter(graph)
    case LatestEstimate => new LatestEstimateRouter(graph)
  }
}

object OrderingType extends Enumeration {
  type OrderingType = Value
  val FIFO, Auction, Pressure = Value

  def make[T <: Ordered[T]](enum: OrderingType.Value) = enum match {
    case FIFO => new FIFO_Ordering[T]()
    case Auction => new AuctionOrdering[T]()
    case Pressure => new PressureOrdering[T]()
  }
}

object WalletType extends Enumeration {
  type WalletType = Value
  val Static, Freerider, Fair, System = Value

  def make(enum: WalletType.Value, budget: Int, priority: Double, bid_ahead: Boolean) = enum match {
    case Static => new StaticWallet(budget, priority)
    case Freerider => new FreeriderWallet(priority)
    case Fair => new FairWallet(budget, priority, bid_ahead)
  }
}

object ReroutePolicyType extends Enumeration {
  type ReroutePolicyType = Value
  val Never, Regularly, PriceChange = Value

  def make(enum: ReroutePolicyType.Value, a: Agent) = enum match {
    case Never => new NeverReroutePolicy(a)
    case Regularly => new RegularlyReroutePolicy(a)
    case PriceChange => new PriceChangeReroutePolicy(a)
  }
}

object ScenarioUtil {
  def diff[T](a: T, b: T, header: String = ""): Option[String] =
    if (a == b)
      None
    else
      Some(s"$header ($a -> $b)")

  // Describe the percentages of each thing
  def percentages[T](things: Iterable[T]) {
    val count = new mutable.HashMap[T, Int]().withDefaultValue(0)
    things.foreach(t => count(t) += 1)
    val total = things.size
    Util.log_push
    for (key <- count.keys) {
      val percent = count(key).toDouble / total * 100.0
      Util.log(f"$key: ${percent}%.2f%%")
    }
    Util.log_pop
  }

  // Min, max, average
  def basic_stats(nums: Iterable[Double]) =
    f"${nums.min}%.2f - ${nums.max}%.2f (average ${nums.sum / nums.size}%.2f)"
  def basic_stats_int(nums: Iterable[Int]) =
    f"${nums.min} - ${nums.max} (average ${nums.sum / nums.size}%.2f)"
}
