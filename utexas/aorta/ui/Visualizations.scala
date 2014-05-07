// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import java.awt.Color

import utexas.aorta.map.{Road, FreeflowRouter}
import utexas.aorta.sim.Simulation
import utexas.aorta.experiments.{ScenarioRoadUsage, DistributionData, PlotUtil}
import utexas.aorta.common.Timer
import utexas.aorta.common.algorithms.PathResult

trait Visualization {
  // TODO could use "self: OrganizerClass =>" pattern...
  private var state: GuiState = null
  private var canvas: MapCanvas = null
  private def sim(): Simulation = canvas.sim  // TODO whys compiler think this is recursive?

  def setup_viz(s: GuiState, c: MapCanvas) {
    state = s
    canvas = c
  }

  def show_pathfinding(from: Road, to: Road) {
    val timer = Timer("Pathfinding")
    // TODO Show each type of route in a different color...
    val colors = List(Color.CYAN, Color.RED, Color.BLUE, Color.GREEN, Color.YELLOW)
    val routers = List(new FreeflowRouter(sim.graph))

    for ((router, color) <- routers.zip(colors)) {
      val route = router.path(from, to).path
      //route.foreach(step => println("  - " + step))
      println(s"for $color, we have $route")
      // TODO a hack, I want to name it properly!
      //state.road_colors.set_layer(s"${router.router_type} pathfinding")
      state.road_colors.add_layer("route")  // to get bold roads
      route.foreach(r => state.road_colors.set("route", r, color))
      state.set_cur_layer("route")
    }
    timer.stop()
    canvas.repaint()
  }

  // percentile is [0, 1]
  def show_heatmap(
    costs: Map[Road, Double], percentile: Double, layer: String, show_histogram: Boolean = false
  ) {
    val sorted_costs = costs.values.toArray.sorted

    if (show_histogram) {
      // TODO this is particular to show_tolls!
      val data = DistributionData(Map(layer -> sorted_costs.filter(_ > 0)), layer, layer)
      new PlotUtil() {
        show(histogram(data))
      }
    }

    // Exclude the top percent of costs; they're usually super high and throw off the visualization
    val max_cost = sorted_costs(math.min(
      sorted_costs.size - 1, (percentile * sorted_costs.size).toInt
    ))
    val heatmap = new Heatmap()
    state.road_colors.add_layer(layer)
    for ((r, cost) <- costs) {
      // Cap at 1 since we may chop off some of the largest values
      state.road_colors.set(layer, r, heatmap.color(math.min(cost / max_cost, 1.0)))
    }
    state.set_cur_layer(layer)
  }

  // Hardcoded to show first component of 2-tuple cost
  def show_path_costs(result: PathResult, percentile: Double = .99) {
    show_heatmap(result.costs.mapValues(_._1), percentile, "path costs")
    for (r <- result.path) {
      state.road_colors.set("path costs", r, Color.GREEN)
    }
    canvas.repaint()
  }

  def show_tolls(percentile: Double = 1) {
    // TODO arbitrary turn for the toll
    val costs = sim.graph.roads.map(r => r -> r.to.intersection.tollbooth.toll(r.to.turns.head)).toMap
    show_heatmap(costs, percentile, "tolls", show_histogram = true)
    canvas.repaint()
  }

  def show_road_usage(percentile: Double = .99) {
    for (fn <- canvas.prompt_fn("Select a road_usage.gz metric from an experiment")) {
      val metric = ScenarioRoadUsage(fn)
      // TODO delta btwn baseline and others. how to handle negatives?
      for (mode <- metric.usages_by_mode.keys) {
        show_heatmap(
          metric.usages_by_mode(mode).map(r => sim.graph.get_r(r.r) -> r.num_drivers.toDouble).toMap,
          percentile, s"road usage in $mode (number of drivers)"
        )
        show_heatmap(
          metric.usages_by_mode(mode).map(r => sim.graph.get_r(r.r) -> r.sum_priority.toDouble).toMap,
          percentile, s"road usage in $mode (sum of driver priority)"
        )
      }
      canvas.repaint()
    }
  }
}
