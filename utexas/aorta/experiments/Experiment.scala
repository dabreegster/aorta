// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import scala.collection.mutable
import java.io.File

import utexas.aorta.map.Graph
import utexas.aorta.sim.{Simulation, EV_Heartbeat, EV_AgentSpawned}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.{ModScenarioTool, Scenario}
import utexas.aorta.ui.GUIDebugger
import utexas.aorta.analysis.SimREPL

import utexas.aorta.common.{RNG, Util, Flags, AgentID, IO, Timer}

// TODO divorce scenario generation from the rest?
case class ExpConfig(
  spawn_per_hour: Int,
  generations: Int,
  report_every_ms: Int,
  deadline: Int,
  map_fn: String,
  gs_prefix: Option[String],
  // If this is set, don't generate a new scenario
  scenario_fn: Option[String]
) {
  // TODO local mode could be automated too, but for now, this works
  def is_automated = gs_prefix.isDefined
}

object ExpConfig {
  private val rng = new RNG()
  private def random_map =
    rng.choose(new File("maps").listFiles.map(_.toString).filter(_.endsWith(".map")))

  private val report_locally_every_ms = 10 * 1000
  private val report_remotely_every_ms = 60 * 1000

  def template = ExpConfig(0, 0, 0, 12 * 3600, random_map, None, None)

  def tiny_test = template.copy(spawn_per_hour = 100, generations = 1)
  def small_local_test = template.copy(spawn_per_hour = 5000, generations = 3)
  def atx_cloud_test = template.copy(
    spawn_per_hour = rng.int(10000, 15000), generations = 3,
    map_fn = rng.choose(Array(
      "maps/austin.map", "maps/baton_rouge.map", "maps/seattle.map", "maps/sf.map"
    )))
  def tollbooth_test = template.copy(
    spawn_per_hour = 8000, generations = 1, map_fn = "maps/austin.map"
  )
  //def safe_atx_cloud_test =
    //template.copy(spawn_per_hour = 10000, generations = 3, map_fn = "maps/austin.map")
  def milo_test = template.copy(
    spawn_per_hour = 15000, generations = 1, map_fn = "maps/baton_rouge.map"
  )

  def from_args(args: Array[String]): ExpConfig = {
    // Empty, just mode, or mode and GS prefix
    if (args.isEmpty) {
      return small_local_test
    } else {
      val base = args.head match {
        case "tiny" => tiny_test
        case "local" => small_local_test
        case "cloud" => atx_cloud_test
        case "tollbooth" => tollbooth_test
        case "milo" => milo_test
        case fn if fn.startsWith("scenarios/") => template.copy(scenario_fn = Some(fn))
        case _ => throw new IllegalArgumentException(s"Dunno mode ${args.head}")
      }
      if (args.tail.isEmpty) {
        return base.copy(report_every_ms = report_locally_every_ms)
      } else {
        return base.copy(
          report_every_ms = report_remotely_every_ms, gs_prefix = args.tail.headOption
        )
      }
    }
  }
}

class Experiment(config: ExpConfig) {
  protected lazy val scenario = get_scenario()
  protected val uid = Util.unique_id
  protected val io = new IO(config.gs_prefix)
  //Flags.set("--savestate", "false")

  protected def scenario_params: Array[String] = Array()
  protected def get_scenario(): Scenario = {
    config.scenario_fn match {
      case Some(fn) => {
        val orig = Scenario.load(fn)
        // Make a copy under the right uid
        val s = orig.copy(name = orig.map_fn.replace("maps/", "scenarios/").replace(".map", "_" + uid))
        s.save()
        return s
      }
      case None => {
        val scenario_fn = config.map_fn.replace("maps/", "scenarios/").replace(".map", "_" + uid)
        io.notify("Generating scenario")
        ModScenarioTool.main(Array(
          config.map_fn, "--out", scenario_fn, "--spawn", config.spawn_per_hour.toString,
          "delay=3600", "lifetime=3600", "generations=" + config.generations
        ) ++ scenario_params)
        return Scenario.load(scenario_fn)
      }
    }
  }

  protected var round = 1
  protected def simulate(sim: Simulation) {
    var last_time = 0L
    sim.listen(classOf[EV_Heartbeat], _ match { case e: EV_Heartbeat => {
      val now = System.currentTimeMillis
      if (now - last_time > config.report_every_ms) {
        last_time = now
        // TODO counts (active agents, moves, CH, and A*) will be wrong... dont reset yet
        io.notify(s"Round $round at ${Util.time_num(sim.tick)}: ${e.describe}")
      }
    }})
    new GUIDebugger(sim) // TODO hafta sys.exit at end if used

    // TODO move this to simulation itself.
    while (!sim.done) {
      sim.step()
      if (sim.tick >= config.deadline) {
        round += 1
        throw new Exception(s"Simulation past ${config.deadline} seconds. Giving up")
      }
    }
    round += 1
  }
}

// The future! All experiments should be rewritten to have this form, probably
abstract class SmartExperiment(config: ExpConfig, val name: String) extends Experiment(config) {
  protected def run(): Unit
  protected def get_metrics(info: MetricInfo): List[Metric]

  def run_experiment() {
    run()
    io.notify("Success")
  }

  protected def run_trial(s: Scenario, mode: String): List[Metric] = {
    val sim = s.make_sim().setup()
    val metrics = get_metrics(MetricInfo(sim, mode, io, uid, name))
    try {
      val t = Timer(s"round $round ($mode)")
      simulate(sim)
      t.stop()
    } catch {
      case e: Throwable => {
        e.printStackTrace()
        io.notify(s"BORKED - $e")
        val name = "buggy_" + sim.graph.basename + "_" + mode
        s.copy(name = name).save()
        // TODO also upload latest savestate, and name everything reasonably
        io.upload(name)
        if (!config.is_automated) {
          // Drop into a REPL to allow for manual diagnosis
          new SimREPL(sim).run()
        }
        throw e
      }
    }
    return metrics
  }

  protected def output_data(results: List[List[Metric]]) {
    Util.log("All simulations over, dumping metrics now...")
    Util.mkdir(results.head.head.info.base_dir)

    val mode_order = results.map(ls => ls.head.mode)
    for ((name, metrics) <- results.flatten.groupBy(_.name)) {
      val metrics_by_mode = metrics.map(m => m.mode -> m).toMap
      // Sort into a canonical mode order, which should come from the results
      metrics.head.output(mode_order.map(m => metrics_by_mode(m)))
    }
  }
}
