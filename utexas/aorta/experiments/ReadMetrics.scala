// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import scala.io.Source
import scala.collection.mutable
import java.util.zip.GZIPInputStream
import java.io.{BufferedInputStream, FileInputStream, File}

import utexas.aorta.common.{Util, RoadID}

trait MetricReader {
  def read(fn: String) =
    if (fn.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(fn))))
    else
      Source.fromFile(fn)

  def read_times(fn: String): ScenarioTimes = {
    val lines = read(fn).getLines
    val header = lines.next.split(" ")
    Util.assert_eq(header.take(3).toList, List("agent", "priority", "ideal_time"))
    return ScenarioTimes(
      ScenarioTag(fn), header.drop(3),
      lines.map(l => TripTimeResult(l.split(" ").map(_.toDouble))).toArray
    )
  }

  def read_distances(fn: String): ScenarioDistances = {
    val lines = read(fn).getLines
    val header = lines.next.split(" ")
    Util.assert_eq(header.take(3).toList, List("agent", "priority", "ideal_distance"))
    return ScenarioDistances(
      ScenarioTag(fn), header.drop(3),
      lines.map(l => TripDistanceResult(l.split(" ").map(_.toDouble))).toArray
    )
  }

  def read_turn_delay(fn: String): ScenarioTurnDelays = {
    val lines = read(fn).getLines
    Util.assert_eq(lines.next, "mode turn_delay_bin count")
    return ScenarioTurnDelays(
      ScenarioTag(fn), lines.map(l => TurnDelayResult(l.split(" "))).toArray
    )
  }

  def read_paths(fn: String): ScenarioPaths = {
    val lines = read(fn).getLines
    val header = lines.next.split(" ")
    Util.assert_eq(header.take(3).toList, List("agent", "priority", "ideal_spawn_time"))
    return ScenarioPaths(
      ScenarioTag(fn), header.drop(3), lines.map(l => AgentPath(l.split(" "))).toArray
    )
  }

  def load(base_dir: String): Summary = {
    val times = read_times(base_dir + "/trip_time.gz")
    val distances = read_distances(base_dir + "/trip_distance.gz")
    val paths = read_paths(base_dir + "/trip_paths.gz")
    val agents = (times.agents, distances.agents, paths.agents).zipped.map({
      case (t, d, p) => AgentSummary(
        t.id, t.ideal_time.toInt, d.ideal_distance.toInt, p.ideal_spawn_time, t.times, d.distances,
        p.paths
      )
    })
    return Summary(times.tag, times.modes, agents, times, distances, paths)
  }
}

// TODO This assumes all the metrics to fill this out were chosen, and right now, it's created
// from the other smaller structures
case class Summary(
  tag: ScenarioTag, modes: Array[String], agents: Array[AgentSummary],
  // TODO Only included so methods in Results can work. Re-think.
  time: ScenarioTimes, distance: ScenarioDistances, path: ScenarioPaths
) {
  def apply(agent: Int) = agents(agent)
  def quant_scores() {
    val real_time = time.toss_outliers().add_ideal
    val real_modes = ("ideal" :: modes.toList).toArray

    val raw_unweighted_sums = real_modes.zipWithIndex.map({ case (mode, idx) =>
      mode -> real_time.agents.map(_.times(idx)).sum
    }).toMap
    val raw_weighted_sums = real_modes.zipWithIndex.map({ case (mode, idx) =>
      mode -> real_time.agents.map(a => a.priority * a.times(idx)).sum
    }).toMap
    val ratio_unweighted_sums = real_modes.zipWithIndex.map({ case (mode, idx) =>
      mode -> real_time.agents.map(a => a.times(idx) / a.ideal_time).sum
    }).toMap
    val ratio_weighted_sums = real_modes.zipWithIndex.map({ case (mode, idx) =>
      mode -> real_time.agents.map(a => a.priority * a.times(idx) / a.ideal_time).sum
    }).toMap
    val sums = List(raw_unweighted_sums, raw_weighted_sums, ratio_unweighted_sums, ratio_weighted_sums)

    println(s"Mode, raw unweighted, raw weighted, ratio unweighted, ratio weighted")
    for (mode <- real_modes) {
      println(mode + ": " + sums.map(group => "%,.0f".format(group(mode))).mkString(" -- "))
    }
  }
}

case class AgentSummary(
  id: Int, ideal_time: Int, ideal_distance: Int, ideal_spawn_time: Double,
  times: Array[Double], distances: Array[Double], paths: Array[List[Crossing]]
) {
  override def toString(): String = {
    val s = new mutable.StringBuilder()
    s ++= s"Agent $id\n"
    s ++= s"  Time: ideal $ideal_time, actual "
    s ++= times.map(t => f"$t (${t / ideal_time}%.2fx)").mkString(", ") + "\n"
    s ++= s"  Distance: ideal $ideal_distance, actual "
    s ++= distances.map(d => f"$d (${d / ideal_distance}%.2fx)").mkString(", ") + "\n"
    for ((path, idx) <- paths.zipWithIndex) {
      s ++= s"  Path times ($idx): " + path.map(c => c.exit - c.entry).mkString(", ") + "\n"
    }
    return s.toString
  }
}

case class ScenarioTag(experiment: String, id: String, map: String)

case class TripTimeResult(id: Int, priority: Double, ideal_time: Double, times: Array[Double])
case class TripDistanceResult(id: Int, priority: Double, ideal_distance: Double, distances: Array[Double])
case class TurnDelayResult(mode: String, bin: Double, count: Double)
case class Crossing(road: Int, entry: Double, exit: Double)
case class AgentPath(id: Int, priority: Double, ideal_spawn_time: Double, paths: Array[List[Crossing]])
case class RoadUsage(r: RoadID, mode: String, num_drivers: Int, sum_priority: Double)

case class ScenarioTimes(tag: ScenarioTag, modes: Array[String], agents: Array[TripTimeResult]) {
  def toss_outliers(ratio_cap: Double = 1000) = copy(agents = agents.filter(a =>
    !a.times.exists(t => t / a.ideal_time > ratio_cap)
  ))
  // As the first mode
  def add_ideal = copy(
    modes = ("ideal" :: modes.toList).toArray,
    agents = agents.map(t => t.copy(times = (t.ideal_time :: t.times.toList).toArray))
  )
}
case class ScenarioDistances(tag: ScenarioTag, modes: Array[String], agents: Array[TripDistanceResult])
case class ScenarioTurnDelays(tag: ScenarioTag, delays: Array[TurnDelayResult])
case class ScenarioPaths(tag: ScenarioTag, modes: Array[String], agents: Array[AgentPath])
case class ScenarioRoadUsage(tag: ScenarioTag, usages_by_mode: Map[String, List[RoadUsage]])

// TODO stuff here is the dual of stuff in Metrics. pair them together somehow?
object ScenarioTag {
  // fn is a possibly full path of the form "/.../experiment_id_map/something_final"
  def apply(fn: String): ScenarioTag = {
    // Passing fn through File canonicalizes the path and gets rid of double //'s
    val items = new File(fn).getPath.split("/")
    val pieces = items(items.size - 2).split("_")
    // The city piece could have _'s in it
    return new ScenarioTag(pieces(0), pieces(1), pieces.drop(2).mkString("_"))
  }
}
object TripTimeResult {
  def apply(fields: Array[Double]) = new TripTimeResult(
    fields(0).toInt, fields(1), fields(2), fields.drop(3)
  )
}
object TripDistanceResult {
  def apply(fields: Array[Double]) = new TripDistanceResult(
    fields(0).toInt, fields(1), fields(2), fields.drop(3)
  )
}
object TurnDelayResult {
  def apply(fields: Array[String]) =
    new TurnDelayResult(fields(0), fields(1).toDouble, fields(2).toDouble)
}
object AgentPath {
  def apply(fields: Array[String]) = new AgentPath(
    fields(0).toInt, fields(1).toDouble, fields(2).toDouble,
    fields.drop(3).map(s => Crossing.list(s)).toArray
  )
}
object Crossing {
  def list(raw: String) = raw.split(",").grouped(3).map(triple => single(triple)).toList
  def single(triple: Array[String]) = Crossing(triple(0).toInt, triple(1).toDouble, triple(2).toDouble)
}
object RoadUsage {
  def apply(fields: Array[String]) = new RoadUsage(
    new RoadID(fields(0).toInt), fields(1), fields(2).toInt, fields(3).toDouble
  )
}

object ScenarioRoadUsage extends MetricReader {
  def apply(fn: String): ScenarioRoadUsage = {
    val lines = read(fn).getLines
    val header = lines.next
    Util.assert_eq(header, "road mode num_drivers sum_priority")
    val usages = lines.map(l => RoadUsage(l.split(" "))).toList
    return ScenarioRoadUsage(ScenarioTag(fn), usages.groupBy(_.mode))
  }
}
