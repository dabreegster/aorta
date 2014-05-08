// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.make

import utexas.aorta.map.Graph
import utexas.aorta.experiments.ScenarioPresets

import utexas.aorta.common.{Util, RNG, cfg, RoadID}

// Command-line interface
object ModScenarioTool {
  // TODO generate choices for things!
  val usage =
    ("scenarios/foo --out scenarios/new_foo [--vert ...]* [--agent ...]* [--spawn ...]* [--cfg_wallets ...]\n" +
     "  --vert 42 policy=StopSign ordering=FIFO\n" +
     "  --agent 3 start=0 end=100 time=16.2 route=Drunken wallet=Random budget=90\n" +
     "  --spawn 500 start=0 end=100 time=16.2 generations=1 lifetime=3600 route=Drunken wallet=Random budget=90\n" +
     "  --all_verts policy=StopSign ordering=FIFO\n" +
     "\n" +
     "Or pass two scenarios to get a diff")
     // TODO removing all existing agents, or something

  private def dump_usage() {
    Util.log(usage)
    sys.exit
  }

  // TODO split it up a bit for readability
  def main(arg_array: Array[String]) {
    if (arg_array.size == 2) {
      val Array(a1, a2) = arg_array
      Util.log(s"Diffing $a1 with $a2\n")
      val s1 = Scenario.load(a1)
      val s2 = Scenario.load(a2)
      s1.diff(s2)
      sys.exit
    }

    var args = arg_array.toList
    def shift_args(): String = {
      if (args.isEmpty) {
        dump_usage
      }
      val head = args.head
      args = args.tail
      return head
    }

    def slurp_params(): Map[String, String] = {
      val pair = args.span(a => !a.startsWith("--"))
      args = pair._2
      return pair._1.map(p => {
        val Array(k, v) = p.split("=")
        k -> v
      }).toMap
    }

    // If they pass in a map instead, make an empty scenario
    val input = shift_args
    var s: Scenario = null
    // the logging will be ugly if this gets initialized in the middle of
    // something
    lazy val graph = Graph.load(s.map_fn)

    try {
      s = Scenario.load(input)
    } catch {
      case _: Throwable => {
        Util.log(s"Initializing empty scenario on $input...")
        s = Scenario(
          s"scenarios/empty_${input}", input, Array(), Array(),
          SystemWalletConfig()
        )
        s = s.copy(
          intersections = IntersectionDistribution.default(graph)
        )
      }
    }

    val rng = new RNG()
    var changed_output = false

    while (args.nonEmpty) {
      shift_args match {
        case "--out" => {
          s = s.copy(name = shift_args)
          changed_output = true
        }
        // --vert 42 policy=StopSign ordering=FIFO
        case "--vert" => {
          val id = shift_args.toInt
          val old_v = s.intersections(id)
          val new_v = mod_vert(old_v, slurp_params)
          old_v.diff(new_v)
          s.intersections(id) = new_v
        }
        // --agent 3 start=0 end=100 time=16.2 route=Drunken wallet=Random budget=90
        case "--agent" => {
          val id = shift_args.toInt
          val old_a = s.agents(id)
          val new_a = mod_agent(old_a, slurp_params, graph, rng)
          old_a.diff(new_a)
          s.agents(id) = new_a
        }
        // TODO specifying ranges or choices for stuff.
        // --spawn 500 start=area_file end=area_file time=16.2 generations=1 lifetime=3600 route=Drunken wallet=Random budget=90
        case "--spawn" => {
          val number = shift_args.toInt
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set(
            "starts", "ends", "delay", "generations", "lifetime", "route",
            "wallet", "budget"
          ))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --spawn")
            sys.exit
          }

          val starts = params.get("starts") match {
            case Some(fn) => {
              val r = Util.reader(fn)
              Range(0, r.int).map(_ => graph.roads(r.int)).toArray
            }
            case None => graph.roads
          }
          val ends = params.get("ends") match {
            case Some(fn) => {
              val r = Util.reader(fn)
              Range(0, r.int).map(_ => graph.roads(r.int)).toArray
            }
            case None => graph.roads
          }
          val delay = params.get("delay") match {
            case Some(t) => t.toDouble
            case None => 0.0
          }
          val generations = params.get("generations") match {
            case Some(t) => t.toInt
            case None => 1
          }
          val lifetime = params.get("lifetime") match {
            case Some(t) => t.toDouble
            case None => 3600.0
          }
          val wallet = Array(
            WalletType.withName(params.getOrElse("wallet", cfg.wallet))
          )
          val budget = params.get("budget") match {
            case Some(t) => {
              val Array(a, b) = t.split("-")
              (a.toInt, b.toInt)
            }
            case None => (0, 10)
          }

          // TODO describe more?
          Util.log(s"Adding $number * $generations new agents")
          for (generation <- Range(0, generations)) {
            val time = (generation * lifetime, generation * lifetime + delay)
            val new_agents = AgentDistribution.uniform_times(
              Range(s.agents.size, s.agents.size + number), starts, ends, time, wallet, budget
            )
            s = s.copy(agents = s.agents ++ new_agents)
          }
        }
        // --all_verts policy=StopSign ordering=FIFO
        case "--all_verts" => {
          val params = slurp_params
          s = s.copy(intersections = s.intersections.map(old => mod_vert(old, params)))
        }
        // --all_agents wallet=Random budget=1
        case "--all_agents" => {
          val params = slurp_params
          s = s.copy(agents = s.agents.map(old => mod_agent(old, params, graph, rng)))
        }
        case "--cfg_wallets" => {
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set(
            "thruput_bonus", "avail_capacity_threshold", "capacity_bonus",
            "dependency_rate", "waiting_rate", "ready_rate"
          ))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --cfg_wallets")
            sys.exit
          }

          var wallet = s.system_wallet
          params.foreach(pair => wallet = pair match {
            case ("thruput_bonus", x) => wallet.copy(thruput_bonus = x.toInt)
            case ("avail_capacity_threshold", x) => wallet.copy(avail_capacity_threshold = x.toInt)
            case ("capacity_bonus", x) => wallet.copy(capacity_bonus = x.toInt)
            case ("dependency_rate", x) => wallet.copy(dependency_rate = x.toInt)
            case ("waiting_rate", x) => wallet.copy(waiting_rate = x.toInt)
            case ("ready_rate", x) => wallet.copy(ready_bonus = x.toInt)
          })

          Util.log(s"Changing system wallet configuration: $wallet")
          s = s.copy(system_wallet = wallet)
        }
        case "--preset" => {
          val preset = shift_args
          Util.log(s"Using experiment preset $preset")
          s = ScenarioPresets.transform(s, preset)
        }
        case _ => dump_usage
      }
    }
    Util.log("")

    s.summarize()

    if (changed_output) {
      s.save()
      Util.log(s"\nSaved scenario to ${s.name}")
      // TODO warn if overwriting? prompt?
    }
  }

  private def mod_vert(old: MkIntersection, params: Map[String, String]): MkIntersection = {
    val bad_params = params.keys.toSet.diff(Set("policy", "ordering"))
    if (!bad_params.isEmpty) {
      Util.log(s"$bad_params aren't valid params for --vert or --all_verts")
      sys.exit()
    }
    return old.copy(
      policy = IntersectionType.withName(
        params.getOrElse("policy", old.policy.toString)
      ),
      ordering = OrderingType.withName(
        params.getOrElse("ordering", old.ordering.toString)
      )
    )
  }

  private def mod_agent(old: MkAgent, params: Map[String, String], graph: Graph, rng: RNG): MkAgent = {
    val bad_params = params.keys.toSet.diff(Set(
      "start", "time", "end", "wallet", "budget", "orig_router", "rerouter", "reroute_policy"
    ))
    if (!bad_params.isEmpty) {
      Util.log(s"$bad_params aren't valid params for --agent or --all_agents")
      sys.exit()
    }

    // Preserve the original spawning distance, or choose an appropriate
    // new one
    val start_dist = params.get("start") match {
      case Some(r) if r.toInt == old.start.int => old.start_dist
      case Some(r) => graph.roads(r.toInt).rightmost.safe_spawn_dist(rng)
      case None => old.start_dist
    }

    return old.copy(
      // TODO fix all the orElse patterns here
      start = params.get("start").map(e => new RoadID(e.toInt)).getOrElse(old.start),
      start_dist = start_dist,
      birth_tick = params.getOrElse("time", old.birth_tick.toString).toDouble,
      route = old.route.copy(
        orig_router = RouterType.withName(
          params.getOrElse("orig_router", old.route.orig_router.toString)
        ),
        rerouter = RouterType.withName(params.getOrElse("rerouter", old.route.rerouter.toString)),
        goal = params.get("end").map(r => new RoadID(r.toInt)).getOrElse(old.route.goal),
        reroute_policy = ReroutePolicyType.withName(params.getOrElse("reroute_policy", old.route.reroute_policy.toString))
      ),
      wallet = old.wallet.copy(
        policy = WalletType.withName(params.getOrElse("wallet", old.wallet.policy.toString)),
        budget = params.getOrElse("budget", old.wallet.budget.toString).toInt
      )
    )
  }
}
