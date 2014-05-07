// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.immutable
import scala.collection.mutable

import utexas.aorta.map.Coordinate

import utexas.aorta.common.{Util, cfg}

class Pass2_Part3(graph: PreGraph2) {
  // Find vertices with exactly two roads involved. They're redundant.
  def run() {
    Util.log("Collapsing degenerate vertices...")
    var verts = immutable.TreeMap.empty[Coordinate, mutable.TreeSet[PreEdge2]]
    def addBinding(pt: Coordinate, e: PreEdge2) {
      if (!verts.contains(pt)) {
        verts += ((pt, new mutable.TreeSet[PreEdge2]()))
      }
      verts(pt) += e
    }
    def removeBinding(pt: Coordinate, e: PreEdge2) {
      if (verts.contains(pt)) {
        verts(pt) -= e
        if (verts(pt).isEmpty) {
          verts -= pt
        }
      }
    }

    for (e <- graph.edges) {
      addBinding(e.from, e)
      addBinding(e.to, e)
    }

    // TODO whys this a fixpoint?
    var changed = true
    while (changed) {
      verts.find(v => v._2.size == 2) match {
        case Some((_, roads)) => {
          val r1 = roads.head
          val r2 = roads.tail.head
          removeBinding(r1.from, r1)
          removeBinding(r1.to, r1)
          removeBinding(r2.from, r2)
          removeBinding(r2.to, r2)
          graph.edges = graph.edges.filter(e => e != r1 && e != r2)

          // TODO if type, lanes, or oneway doesnt match, bail out
          val replace = merge_roads(r1, r2)
          graph.edges += replace
          addBinding(replace.from, replace)
          addBinding(replace.to, replace)
        }
        case None => changed = false
      }
    }
  }

  // What's the internal vertex? Destroy it.
  private def merge_roads(r1: PreEdge2, r2: PreEdge2): PreEdge2 = {
    val dat = merge_dat(r1.dat, r2.dat)
    if (r1.from == r2.from) {
      return new PreEdge2(r1.to, r2.to, r1.points.reverse ++ r2.points.tail, dat)
    } else if (r1.from == r2.to) {
      return new PreEdge2(r2.from, r1.to, r2.points ++ r1.points.tail, dat)
    } else if (r1.to == r2.from) {
      return new PreEdge2(r1.from, r2.to, r1.points ++ r2.points.tail, dat)
    } else if (r1.to == r2.to) {
      return new PreEdge2(r1.from, r2.from, r1.points ++ r2.points.reverse.tail, dat)
    } else {
      throw new Exception(s"$r1 and $r2 not mergable!")
    }
  }

  private def merge_dat(r1: PreEdge1, r2: PreEdge1) = new PreEdge1(
    (if (r1.name == r2.name)
      r1.name
    else
      r1.name + " / " + r2.name),
    r1.road_type,    // TODO diff types?
    r1.oneway,       // TODO cant merge if different
    r1.orig_id,      // arbitrarily choose one
    null,            // no points matter now
    r1.lanes         // TODO diff num of lanes?
  )
}
