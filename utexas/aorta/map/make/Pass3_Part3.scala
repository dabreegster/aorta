// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable

import utexas.aorta.map.{Edge, Turn, Traversable, Road}

import utexas.aorta.common.{Util, cfg, TurnID, RoadID, EdgeID, VertexID}

class Pass3_Part3(graph: PreGraph3) {
  // for tarjan's
  private val visited = new mutable.HashSet[Traversable]
  private val t_idx = new mutable.HashMap[Traversable, Int]
  private val t_low = new mutable.HashMap[Traversable, Int]
  private val in_stack = new mutable.HashSet[Traversable]  // yay linear-time Tarjan's
  private val t_stack = new mutable.Stack[Traversable]
  private var dfs = 0   // counter numbering

  def run() {
    Util.log("Removing disconnected chunks of the network")
    var changed = true
    Util.log_push()
    while (changed) {
      // first find edges with no predecessors/successors, remove them, and
      // flood that effect out...
      val change1 = clean_half_edges()
      // then finds SCCs and removes all but the largest
      val change2 = clean_disconnected()
      // since each phase potentially affects the other, repeat both until
      // nothing changes
      changed = change1 || change2

      // reset state for tarjan's
      visited.clear()
      t_idx.clear()
      t_low.clear()
      in_stack.clear()
      t_stack.clear()
      dfs = 0
    }
    Util.log_pop()
  }

  // Returns true if any edges are removed
  private def clean_half_edges(): Boolean = {
    val orig_edges = graph.edges.size
    val orig_verts = graph.vertices.size
    val orig_roads = graph.roads.size

    Util.log("Using fixpoint algorithm to prune half-edges")

    // fixpoint algorithm: find half-edges till there are none
    // TODO flooding would be faster.
    var done = false
    var any_changes = false
    while (!done) {
      graph.edges.partition(e => e.doomed) match {
        case (bad, good) => {
          // TODO cant pattern match nil for mutable list :(
          if (bad.isEmpty) {
            done = true
          } else {
            graph.edges = good
            remove_turns_to_bad_edges(bad.toSet)
            any_changes = true
          }
        }
      }
    }

    if (any_changes) {
      fix_map()

      Util.log("%,d -> %,d edges, %,d -> %,d vertices, %,d -> %,d roads".format(
        orig_edges, graph.edges.size, orig_verts, graph.vertices.size, orig_roads, graph.roads.size
      ))
    }
    return any_changes
  }

  // Returns true if any edges are removed
  private def clean_disconnected(): Boolean = {
    // use Tarjan's to locate all SCC's in the graph. ideally we'd just
    // have one, but crappy graphs, weird reality, and poor turn heuristics mean
    // we'll have disconnected portions.
    // TODO simplify by flooding Roads, not turns and lanes
    val sccs = new mutable.ListBuffer[List[Traversable]]

    for (t <- graph.traversables) {
      if (!visited(t)) {
        tarjan_body(t, sccs)
      }
    }

    // deal with all edges of all but the largest SCC by either
    // 1) coloring them so we can manually inspect
    // 2) or removing the associated roads, vertices, edges

    // Collect all the bad edges and turns.
    val bad_edges = new mutable.ListBuffer[Edge]
    val bad_turns = new mutable.ListBuffer[Turn]

    sccs.sortBy(scc => scc.size).toList.reverse match {
      // TODO dont return inside here
      case (biggest :: Nil) => return false // Good, just one SCC
      case (biggest :: doomed_sccs) => {
        for (scc <- doomed_sccs; trav <- scc) {
          trav match {
            case e: Edge => bad_edges += e
            case t: Turn => bad_turns += t
          }
        }
      }
      case Nil => throw new Exception("Tarjan saw empty map!")
    }

    Util.log(
      "%,d edges, %,d turns belonging to small SCC".format(bad_edges.size, bad_turns.size)
    )
    val doomed_edges = bad_edges.toSet
    // As a note, all of these steps that completely delete a structure are
    // safe -- anything else referring to them will also be deleted, thanks to
    // Mr. Tarjan.

    // TODO write these at set differences..
    graph.edges = graph.edges.filter(e => !doomed_edges.contains(e))
    val doomed_turns = bad_turns.toSet
    for (v <- graph.vertices) {
      v.turns = v.turns.filter(t => !doomed_turns.contains(t))
    }

    fix_map()
    return true
  }

  // call at the beginning of the recursion to do work on 't' just once. returns
  // the list of traversables to look at next.
  private def tarjan_head(t: Traversable): List[Traversable] = {
    visited += t
    t_idx(t) = dfs
    t_low(t) = dfs
    dfs += 1
    t_stack.push(t)
    in_stack += t

    return t.leads_to
  }

  private def tarjan_body(orig_trav: Traversable, sccs: mutable.ListBuffer[List[Traversable]]): Unit =
  {
    // tuple is ('t', our trav 'backref' so we can do t_low, then list of
    // connected travss left to process)
    val work = new mutable.Stack[(Traversable, Traversable, List[Traversable])]

    // seed with original work
    work.push((orig_trav, null, tarjan_head(orig_trav)))

    while (work.nonEmpty) {
      val (t, backref, next_steps) = work.pop

      if (next_steps.nonEmpty) {
        val next = next_steps.head

        // either way, there's more work to do -- push it on BEFORE other work
        // we might add.
        work.push((t, backref, next_steps.tail))

        if (!visited(next)) {
          // here's where we "recurse"
          work.push((next, t, tarjan_head(next)))
        } else if (in_stack(next)) {
          // here's a back-edge. keeping this map above is the trick that makes
          // Tarjan's O(n)
          t_low(t) = math.min(t_low(t), t_idx(next))
        }

      } else {
        // done with processing all the vert's connections...

        // are we a 'root'?
        if (t_low(t) == t_idx(t)) {
          // pop stack and stop when we hit t
          // these all make an scc
          var member: Traversable = null
          // TODO a functional way? :P
          val scc = new mutable.ListBuffer[Traversable]
          do {
            member = t_stack.pop
            in_stack -= member
            scc += member
          } while (t != member)
          // TODO it'd be awesome to keep the list in sorted order as we build it
          sccs += scc.toList
        }

        // this is normally where we'd return and pop out of the recursion, so
        // do that work here...

        // should only be null for orig_trav
        if (backref != null) {
          t_low(backref) = math.min(t_low(backref), t_low(t))
        }
      }
    }
  }

  private def remove_turns_to_bad_edges(bad_edges: Set[Edge]) {
    for (v <- graph.vertices) {
      v.turns = v.turns.filter(
        t => !bad_edges.contains(t.from) && !bad_edges.contains(t.to)
      )
    }
  }

  // After eating away at elements from edges/vertices/roads, cleanup references
  // to those deleted objects elsewhere.
  private def fix_map() {
    val good_edges = graph.edges.toSet

    // TODO refactor this: squeeze together lane numbers
    // This will end up looking weird (gaps in roads)
    for (r <- graph.roads) {
      val lanes = r.lanes.filter(e => good_edges.contains(e))
      r.lanes.clear()
      r.lanes ++= lanes
      for ((lane, idx) <- r.lanes.zipWithIndex) {
        lane.lane_num = idx
      }
    }

    // See what vertices now have no turns or lanes left
    graph.vertices.partition(v => v.turns.isEmpty) match {
      case (bad, good) => {
        graph.vertices = good
        val bad_set = bad.toSet
        graph.roads = graph.roads.filter(
          r => !bad_set.contains(r.v1) && !bad_set.contains(r.v2) && r.lanes.nonEmpty
        )
      }
    }
  }
}
