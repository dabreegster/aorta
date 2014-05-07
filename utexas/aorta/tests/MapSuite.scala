// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import scala.collection.mutable

import utexas.aorta.map.{Graph, Traversable}
import utexas.aorta.common.cfg

// TODO Use ScalaTests or another framework.

object MapSuite {
  def main(args: Array[String]) {
    val graph = Graph.load(args.head)
    check_emptiness(graph)
    check_connectivity(graph)
    check_turn_conflicts(graph)
    check_geometry(graph)
    check_degenerate_verts(graph)
  }

  def oops(msg: String) {
    //throw new Exception(msg)
    println(msg)
  }

  // Make sure things aren't empty
  def check_emptiness(g: Graph) {
    for (r <- g.roads) {
      if (r.lanes.isEmpty) {
        oops(s"$r has no lanes")
      }
    }

    for (v <- g.vertices) {
      if (v.turns.isEmpty) {
        oops(s"$v has no turns")
      }
    }
  }

  // Every edge should be reachable from every other edge, barring approaches
  // that try to lane-change in too-short areas.
  def check_connectivity(g: Graph) {
    // This is a weak check that just looks at individual edges, not connected
    // components of the whole graph.
    for (e <- g.edges) {
      if (e.next_turns.isEmpty) {
        oops(s"$e leads nowhere")
      }
      if (e.prev_turns.isEmpty) {
        oops(s"Nothing leads to $e")
      }
    }

    // This is another weak check that just floods from some random edge and
    // sees how many edges it reaches. It doesn't guarantee total connectedness,
    // but we'll trust Tarjan's algorithm in pass 3 of construction for now.
    val visit_me = new mutable.Stack[Traversable]
    val visited = new mutable.HashSet[Traversable]
    val start = g.edges.head
    visit_me.push(start)
    visited += start
    
    while (visit_me.nonEmpty) {
      val current = visit_me.pop
      for (next <- current.leads_to if !visited.contains(next)) {
        visited += next
        visit_me.push(next)
      }
    }

    // Did we see everything?
    val size1 = visited.size
    val size2 = g.traversables.size
    if (size1 != size2) {
      oops(
        s"Reached $size1 traversables from $start, but should have found $size2"
      )
    }
  }

  // Conflicts should be symmetric.
  def check_turn_conflicts(g: Graph) {
    for (e <- g.edges) {
      for (t1 <- e.next_turns) {
        for (t2 <- t1.conflicts) {
          if (!t2.conflicts_with(t1)) {
            oops(s"Asymmetric turn conflict between $t1 and $t2")
          }
        }
      }
    }
  }

  // OSM leads to short edges and long turns, but impose some limits.
  def check_geometry(g: Graph) {
    for (e <- g.edges) {
      // TODO cul-de-sacs? edges might be a bit shorter than roads?
      if (e.length <= cfg.min_road_len || e.length.isNaN) {
        oops(s"$e has length ${e.length}. The road has length ${e.road.length}")
      }
    }

    // Make sure cached length is correct.
    g.traversables.foreach(t => {
      val l = t.lines.foldLeft(0.0)((a, b) => a + b.length)
      if (l != t.length) {
        oops(s"$t has recorded length ${t.length} and computed $l")
      }
    })
  }

  // These shouldn't exist
  def check_degenerate_verts(g: Graph) {
    // TODO how to reformulate for directed roads?
    /*val silly = g.vertices.filter(v => v.roads.size == 2)
    if (silly.nonEmpty) {
      oops("Degenerate vertices: " + silly.toList)
    }*/
  }
}
