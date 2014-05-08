// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.sim.{Simulation, EV_AgentSpawned, EV_Reroute, EV_AgentQuit, EV_TurnFinished,
                         EV_IntersectionOutcome, EV_Transition}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.{Scenario, MkAgent}
import utexas.aorta.map.{Edge, Road, Turn}
import utexas.aorta.common.{AgentID, IO, Util, BinnedHistogram}

import scala.collection.mutable

case class MetricInfo(sim: Simulation, mode: String, io: IO, uid: String, experiment: String) {
  def base_dir = experiment + "_" + uid + "_" + sim.graph.basename
}

// One per trial
abstract class Metric(val info: MetricInfo) {
  def name: String
  // Really should be in the companion object, and the type should indicate they're all the same.
  def output(ls: List[Metric])
  def mode = info.mode

  protected def fn = info.base_dir + "/" + name
}

// Record one double per agent
abstract class SinglePerAgentMetric[T](info: MetricInfo) extends Metric(info) {
  protected val per_agent = new mutable.HashMap[AgentID, T]()
  def apply(a: AgentID) = per_agent(a)

  // These are invariant of trial and printed before the per-agent metric
  protected def extra_fields: List[String] = Nil
  protected def extra_data(a: MkAgent): List[Double] = Nil

  override def output(ls: List[Metric]) {
    val f = info.io.output_file(fn)
    f.println(("agent" :: extra_fields ++ ls.map(_.mode)).mkString(" "))
    for (a <- info.sim.scenario.agents) {
      f.println((
        a.id :: extra_data(a) ++ ls.map(_.asInstanceOf[SinglePerAgentMetric[T]].per_agent(a.id))
      ).mkString(" "))
    }
    f.close()
    info.io.done(fn)
  }
}

// Too many values? Throw em into bins and count the size of each bin.
abstract class HistogramMetric(info: MetricInfo, width: Double) extends Metric(info) {
  protected val histogram = new BinnedHistogram(width)

  override def output(ls: List[Metric]) {
    val f = info.io.output_file(fn)
    f.println(s"mode ${name}_bin count")
    for (raw_metric <- ls) {
      val metric = raw_metric.asInstanceOf[HistogramMetric]
      for (bin <- metric.histogram.bins) {
        f.println(List(metric.mode, (bin * width).toInt, metric.histogram(bin)).mkString(" "))
      }
    }
    f.close()
    info.io.done(fn)
  }
}

// Measure how long each agent's trip takes
class TripTimeMetric(info: MetricInfo) extends SinglePerAgentMetric[Double](info) {
  override def name = "trip_time"

  override def extra_fields = List("priority", "ideal_time")
  override def extra_data(a: MkAgent) = List(a.wallet.priority, a.ideal_time(info.sim.graph))

  // First fill out the actual spawn time
  info.sim.listen(classOf[EV_AgentSpawned], _ match {
    case EV_AgentSpawned(a) => per_agent(a.id) = a.sim.tick
  })
  // Then determine the trip time when they finish
  info.sim.listen(classOf[EV_AgentQuit], _ match {
    case e: EV_AgentQuit => per_agent(e.agent.id) = e.agent.sim.tick - per_agent(e.agent.id)
  })
}

// Measure how far each agent travels
class TripDistanceMetric(info: MetricInfo) extends SinglePerAgentMetric[Double](info) {
  override def name = "trip_distance"

  // ideal_distance is the distance of the ideal path, which is the shortest by freeflow time and
  // not length. So distance ratios < 1 happen when a driver takes a shorter distance route that has
  // lower speed limits.
  override def extra_fields = List("priority", "ideal_distance")
  override def extra_data(a: MkAgent) = List(a.wallet.priority, a.ideal_distance(info.sim.graph))

  info.sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, from: Turn, to: Edge) => per_agent(a.id) += to.road.length
    // Don't miss the first road
    case EV_Transition(a, from: Edge, to: Turn) if !per_agent.contains(a.id) =>
      per_agent(a.id) = from.road.length
    case _ =>
  })
  // Handle the case of agents spawning where they end and never firing a transition
  // TODO make EV_Transition fire when agents spawn?
  info.sim.listen(classOf[EV_AgentQuit], _ match {
    case e: EV_AgentQuit if !per_agent.contains(e.agent.id) =>
      per_agent(e.agent.id) = e.agent.at.on.asInstanceOf[Edge].road.length
    case _ =>
  })
}

// Measure how long a driver follows their original route.
class OriginalRouteMetric(info: MetricInfo) extends SinglePerAgentMetric[Double](info) {
  override def name = "orig_routes"

  private val first_reroute_time = new mutable.HashMap[AgentID, Double]()
  info.sim.listen(classOf[EV_Reroute], _ match {
    case EV_Reroute(a, _, false, _, _, _) if !first_reroute_time.contains(a.id) =>
      first_reroute_time(a.id) = a.sim.tick
    case _ =>
  })
  // per_agent is [0, 100]
  info.sim.listen(classOf[EV_AgentQuit], _ match { case e: EV_AgentQuit =>
    per_agent(e.agent.id) =
      100.0 * ((first_reroute_time.getOrElse(e.agent.id, e.end_tick) - e.birth_tick) / e.trip_time)
  })
}

// Measure how much money the agent actually spends of their total budget
class MoneySpentMetric(info: MetricInfo) extends SinglePerAgentMetric[Double](info) {
  override def name = "money_spent"

  info.sim.listen(classOf[EV_AgentQuit], _ match {
    case e: EV_AgentQuit => per_agent(e.agent.id) = e.total_spent
  })
}

// Measure how long drivers wait at intersections, grouped by intersection type
// TODO multiple HistogramMetric. print intersection_type
class TurnDelayMetric(info: MetricInfo) extends HistogramMetric(info, 5.0) {
  override def name = "turn_delay"

  info.sim.listen(classOf[EV_TurnFinished], _ match {
    // could be accept_delay
    case e: EV_TurnFinished => histogram.add(e.total_delay)
  })
}

// Measure how congested roads are when agents enter them
class RoadCongestionMetric(info: MetricInfo) extends HistogramMetric(info, 10.0) {
  override def name = "road_congestion"

  info.sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, _, to: Edge) => histogram.add(to.road.freeflow_percent_full)
    case _ =>
  })
}

// Measure how much competition is present at intersections
// TODO multiple HistogramMetric. print intersection_type
class TurnCompetitionMetric(info: MetricInfo) extends HistogramMetric(info, 1.0) {
  override def name = "turn_competition"

  info.sim.listen(classOf[EV_IntersectionOutcome], _ match {
    case EV_IntersectionOutcome(policy, losers) => histogram.add(losers.size)
  })
}

// Record (road ID, entry time, exit time) for every road each driver crosses
// Encode as a string, sadly
class TripPathsMetric(info: MetricInfo) extends SinglePerAgentMetric[mutable.StringBuilder](info) {
  override def name = "trip_paths"

  override def extra_fields = List("priority", "ideal_spawn_time")
  override def extra_data(a: MkAgent) = List(a.wallet.priority, a.birth_tick)

  // EV_Transition isn't fired for initial entry
  info.sim.listen(classOf[EV_AgentSpawned], _ match {
    case EV_AgentSpawned(a) => per_agent(a.id) = new StringBuilder(
      a.at.on.asEdge.road.id.int + "," + a.sim.tick + ","
    )
  })

  info.sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, from: Edge, to: Turn) => per_agent(a.id) ++= a.sim.tick.toString
    case EV_Transition(a, from: Turn, to: Edge) =>
      per_agent(a.id) ++= "," + to.road.id.int + "," + a.sim.tick + ","
    case _ =>
  })

  // EV_Transition isn't fired for exit either
  info.sim.listen(classOf[EV_AgentQuit], _ match {
    case e: EV_AgentQuit => per_agent(e.agent.id) ++= e.agent.sim.tick.toString
  })
}

class RoadUsageMetric(info: MetricInfo) extends Metric(info) {
  protected val num_drivers = new mutable.HashMap[Road, Int]()
  protected val sum_priority = new mutable.HashMap[Road, Double]()
  for (r <- info.sim.graph.roads) {
    num_drivers(r) = 0
    sum_priority(r) = 0
  }

  override def name = "road_usage"

  override def output(ls: List[Metric]) {
    val f = info.io.output_file(fn)
    f.println("road mode num_drivers sum_priority")
    for (raw_metric <- ls) {
      for (r <- info.sim.graph.roads) {
        val metric = raw_metric.asInstanceOf[RoadUsageMetric]
        f.println(List(r.id, metric.mode, metric.num_drivers(r), metric.sum_priority(r)).mkString(" "))
      }
    }
    f.close()
    info.io.done(fn)
  }

  info.sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, from: Turn, to: Edge) => {
      num_drivers(to.road) += 1
      sum_priority(to.road) += a.wallet.priority
    }
    case _ =>
  })
}
