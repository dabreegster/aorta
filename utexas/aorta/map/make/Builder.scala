// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import utexas.aorta.map.{Graph, Edge, Line}
import utexas.aorta.common.{Util, EdgeID, VertexID, TurnID, RoadID, BinaryStateWriter}

import scala.collection.mutable

object Builder {
  // osm/foo.osm [--disable_pruning] [whitelist_osm]
  def main(args: Array[String]) {
    // TODO real argv parsing
    val use_pruning = args.tail.headOption.getOrElse("") != "--disable_pruning"
    val whitelist = if (use_pruning) args.tail.headOption else args.tail.tail.headOption
    convert(args.head, whitelist, use_pruning)
  }

  // Takes a .osm and returns a .map
  def convert(input: String, whitelist_fn: Option[String], use_pruning: Boolean): String = {
    if (!input.endsWith(".osm")) {
      throw new Exception(s"$input must end with .osm")
    }
    val output = input.replace("osm/", "maps/").replace(".osm", ".map")
    val bldgs = new BuildingScraper()

    // Pass 1
    val pass1 = new Pass1(
      input, whitelist_fn.map(fn => {
        val r = Util.reader(fn)
        Range(0, r.int).map(_ => r.string).toSet
      })
    )
    bldgs.scrape(pass1.osm)
    val graph1 = pass1.run()
    bldgs.normalize_coords(graph1.fix _)
    Graph.set_params(graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale)

    // Pass 2
    val graph2 = new PreGraph2(graph1)
    val artifacts = new Pass2_Part2(graph2).run()
    new Pass2_Part3(graph2).run()

    // Pass 3
    val graph3 = new PreGraph3(graph2)
    new Pass3_Part2(graph3).run()
    if (use_pruning) {
      new Pass3_Part3(graph3).run()
    }
    val remap_lines = new Pass3_Part4(graph3).run()
    bldgs.group(graph3)

    val roads = graph3.roads.toArray
    val graph = new Graph(
      roads, graph3.edges.toArray, graph3.vertices.toArray, artifacts,
      graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale,
      input.replace(".osm", "").replace("osm/", "")
    )
    Util.log(
      s"Dumping map with ${graph3.roads.length} roads, ${graph3.edges.length}" +
      s" edges, and ${graph3.vertices.length} vertices"
    )
    Util.mkdir("maps")
    val w = fix_ids(graph, output, remap_lines._1, remap_lines._2)
    graph.serialize(w)
    w.done()

    return output
  }

  private def fix_ids(graph: Graph, fn: String, first: Map[Edge, Line], last: Map[Edge, Line]): MapStateWriter = {
    val edges = (for ((e, raw_id) <- graph.edges.zipWithIndex)
      yield e.id -> new EdgeID(raw_id)
    ).toMap
    val vertices = (for ((v, id) <- graph.vertices.zipWithIndex)
      yield v.id -> new VertexID(id)
    ).toMap
    val roads = (for ((r, id) <- graph.roads.zipWithIndex)
      yield r.id -> new RoadID(id)
    ).toMap

    return new MapStateWriter(fn, edges, vertices, roads, first, last)
  }
}

// Since we propagate a StateWriter to serialize maps anyway, push the re-mapping of IDs with it.
// TODO dont extend a BinaryStateWriter specifically.
// TODO ideally, have methods for each type of id, and override them.
class MapStateWriter(
  fn: String, val edges: Map[EdgeID, EdgeID], val vertices: Map[VertexID, VertexID],
  val roads: Map[RoadID, RoadID], val first_lines: Map[Edge, Line], val last_lines: Map[Edge, Line]
) extends BinaryStateWriter(fn)
// TODO make turn IDs contig too
