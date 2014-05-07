// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable

import utexas.aorta.map.{Edge, Vertex, Turn, Line, Direction}

import utexas.aorta.common.{Util, cfg, TurnID}

class Pass3_Part4(graph: PreGraph3) {
  private val shortest_first_line = new mutable.HashMap[Edge, Line]
  private val shortest_last_line = new mutable.HashMap[Edge, Line]

  def run(): (Map[Edge, Line], Map[Edge, Line]) = {
    Util.log("Tidying up geometry...")
    var cnt = graph.vertices.size
    for (v <- graph.vertices) {
      adjust_lines(v)
      cnt -= 1
      if (cnt % 100 == 0) {
        print(f"\r  $cnt%,d vertices left to clean up")
      }
    }
    println("")
    return (shortest_first_line.toMap, shortest_last_line.toMap)
  }

  // TODO the rightmost lanes seem to still overlap a bit
  private def adjust_lines(v: Vertex) {
    for (in <- v.in_edges) {
      for (out <- v.out_edges) {
        val l1 = in.lines.last
        val l2 = out.lines.head

        if (!shortest_last_line.contains(in)) {
          shortest_last_line(in) = l1
        }
        if (!shortest_first_line.contains(out)) {
          shortest_first_line(out) = l2
        }

        l1.segment_intersection(l2) match {
          case Some(pt) => {
            val possible1 = new Line(l1.x1, l1.y1, pt.x, pt.y)
            val possible2 = new Line(pt.x, pt.y, l2.x2, l2.y2)

            // This line will intersect many -- store the one that gets
            // trimmed the most.
            if (!shortest_last_line.contains(in) || possible1.length < shortest_last_line(in).length) {
              if (ok_mod(possible1)) {
                shortest_last_line(in) = possible1
              }
            }
            if (!shortest_first_line.contains(out) || possible2.length < shortest_first_line(out).length) {
              if (ok_mod(possible2)) {
                shortest_first_line(out) = possible2
              }
            }
          }
          case _ =>
        }
      }
    }

    // Handle edges with only one line
    for (e <- graph.edges if e.lines.size == 1) {
      (shortest_first_line.get(e), shortest_last_line.get(e)) match {
        case (Some(l1), Some(l2)) => {
          // shortest_first_line will be the one that overrides
          val candidate = new Line(l1.x1, l1.y1, l2.x2, l2.y2)
          if (ok_mod(candidate)) {
            shortest_first_line(e) = candidate
          }
        }
        case _ =>
      }
    }
  }

  // TODO to maybe trim lines a little but not too much, check the total length of the edge
  private def ok_mod(l: Line) = l.length > cfg.end_threshold
}
