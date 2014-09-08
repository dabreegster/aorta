package utexas.aorta.contrib

import utexas.aorta.map.Graph
import utexas.aorta.sim.make._
import utexas.aorta.common.RoadID

// Just an example
object Spawner {
  val map_name = "maps/one-way-world-a-connected.map"
  val scenario_output = "scenarios/connected_test"
  val num_drivers = 5000
  val start_roads = List(
    6, 7
  )
  val end_roads = List(
    0, 1
  )
  val start_times = (0.0, 3600.0)

  def main(args: Array[String]) {
    val graph = Graph.load(map_name)
    // This assigns stop sign / traffic signal / etc to each intersection, so if you want something
    // different, here's where you'd do it
    val intersections = IntersectionDistribution.default(graph)

    val agents = AgentDistribution.uniform_times(
      ids = Range(0, num_drivers),
      starts = start_roads.map(r => graph.get_r(new RoadID(r))).toArray,
      ends = end_roads.map(r => graph.get_r(new RoadID(r))).toArray,
      times = start_times,
      // Ignore these probably
      wallets = Array(AgentDistribution.default_wallet),
      budgets = (0, 100)
    )

    val s = Scenario(scenario_output, map_name, agents, intersections, SimConfig())
    s.save()
  }
}
