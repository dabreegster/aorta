// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

/*package utexas.aorta.sim.meep

import utexas.aorta.map.{Graph, Road}
import utexas.aorta.map.analysis.{AstarRouter, Demand, RouteFeatures}
import utexas.aorta.sim.Agent
import utexas.aorta.common.{Common, AgentID, ValueOfTime}

import scala.collection.mutable

// TODO hackish to have an extra weight there.
case class LinearModel(weights: RouteFeatures, scenario_size_weight: Double, constant: Double) {
  def predict(score: RouteFeatures) =
    weights.score(score) + (scenario_size_weight * Common.scenario.agents.size) + constant
}

case class Predictor(time_model: LinearModel, externality_model: LinearModel) {
  def trip_time(score: RouteFeatures) = time_model.predict(score)
  def externality(score: RouteFeatures) = externality_model.predict(score)
}

case class RouteChoice(
  path: List[Road], score: RouteFeatures, predicted_time: Double,
  predicted_externality: Double
) {
  override def toString = s"Route Choice[time ~ $predicted_time, cost ~ $predicted_externality]"
}

class RouteChooser(graph: Graph, demand: Demand, predictor: Predictor) {
  def discover_routes(start: Road, end: Road, num_routes: Int): List[RouteChoice] = {
    val scores_seen = new mutable.HashSet[RouteFeatures]()
    val result = new mutable.ListBuffer[RouteChoice]()
    // Don't block completely
    var total_attempts_remaining = num_routes * 2
    for (i <- 1 to num_routes) {
      if (i == 1) {
        // Always include the baseline path
        val path = graph.ch_router.path(start, end, Common.tick)
        val score = path
          .map(step => RouteFeatures.for_step(step, demand))
          .fold(RouteFeatures.BLANK)((a, b) => a + b)
        scores_seen += score
        //result += RouteChoice(path, score, predictor.trip_time(score), predictor.externality(score))
        result += RouteChoice(path, score, predictor.trip_time(score), cost(path))
      } else {
        var continue = true
        while (continue && total_attempts_remaining > 0) {
          total_attempts_remaining -= 1
          val router = new AstarRouter(graph, RouteFeatures.random_weight, demand)
          val scored_path = router.scored_path(start, end)
          val path = scored_path._1
          val score = scored_path._2
          if (!scores_seen.contains(score)) {
            scores_seen += score
            result += RouteChoice(
              //path, score, predictor.trip_time(score), predictor.externality(score)
              path, score, predictor.trip_time(score), cost(path)
            )
            continue = false
          }
        }
      }
    }
    return result.toList
  }

  def cost(route: List[Road]) =
    if (route.nonEmpty)
      route.map(_.toll.dollars).max
    else
      0

  def choose_route(choices: List[RouteChoice], vot: ValueOfTime): RouteChoice = {
    val debug = false // TODO

    // The cheapest route is the baseline
    val baseline = choices.minBy(_.predicted_externality)
    val alts = choices.filter(c => c != baseline)
    if (alts.isEmpty) {
      // Sad day.
      return baseline.copy(predicted_externality = 0)
    }

    def rating(r: RouteChoice): ValueOfTime = new ValueOfTime(
      (baseline.predicted_time - r.predicted_time) /
      (r.predicted_externality - baseline.predicted_externality)
    )
    val best = alts.maxBy(c => rating(c).time_per_cost)

    if (debug) {
      println(s"baseline is $baseline")
      for (c <- alts) {
        println(s"  $c")
        println(s"    time savings: ${baseline.predicted_time - c.predicted_time}, cost ${c.predicted_externality - baseline.predicted_externality}, value ${rating(c)}")
        println(s"    features... ${c.score}")
      }
      println(s"best is $best, with rating ${rating(best)}")
      println(s"agents VOT is $vot")
    }

    if (rating(best).time_per_cost >= vot.time_per_cost) {
      // Pay the difference
      return best.copy(
        predicted_externality = best.predicted_externality - baseline.predicted_externality
      )
    } else {
      // Freebie
      return baseline.copy(predicted_externality = 0)
    }
  }
}

// Most of this is completely tmp.
object AgentAdaptor {
  val max_paid = new mutable.HashMap[AgentID, Double]().withDefaultValue(0)
  var special_routes: Set[AgentID] = Nil.toSet

  def reset() {
    max_paid.clear()
  }

  def path(from: Road, to: Road, a: Agent): List[Road] = {
    val choices = Graph.route_chooser.discover_routes(from, to, Graph.num_routes)
    val choice = Graph.route_chooser.choose_route(choices, a.value_of_time)
    max_paid(a.id) = math.max(max_paid(a.id), choice.predicted_externality)
    return choice.path
  }

  def paying_drivers = max_paid.keys.filter(a => max_paid(a) != 0)
}*/
