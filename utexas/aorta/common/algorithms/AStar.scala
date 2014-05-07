// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common.algorithms

import scala.collection.mutable
import java.util

// The algorithms here are generic, but for performance reasons, they are specialized for Roads, the
// only use case in AORTA. TODO investigate @specialized.
// TODO other mostly unused features removed for performance:
// - multiple goals
import utexas.aorta.map.Road
import utexas.aorta.common.Util

case class Pathfind(
  start: Road = null,
  goal: Road = null,
  successors: (Road) => Array[Road] = (step: Road) => step.succs,
  calc_cost: (Road, Road, (Double, Double)) => (Double, Double) = null,
  calc_heuristic: (Road) => (Double, Double) = (node: Road) => (0.0, 0.0),
  add_cost: ((Double, Double), (Double, Double)) => (Double, Double) =
    (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2),
  allow_cycles: Boolean = false,
  cost_start: (Double, Double) = (0, 0),
  banned_nodes: Set[Road] = Set[Road](),
  return_costs: Boolean = false
) {
  def first_succs(succs: Array[Road]) = this.copy(
    successors = (state: Road) => if (state == start) succs else successors(state)
  )
}

case class PathResult(path: List[Road], costs: Map[Road, (Double, Double)], nodes_visited: Int)

class PathfindingFailedException(msg: String) extends Exception(msg)

// TODO perf bug: I think one of the sets calls toString! test with a slow toString
object AStar {
  // T is the node type
  // TODO All costs are pairs of doubles lexicographically ordered right now. Generalize.
  // For performance, if return_costs is false, then the cost map will be empty.
  def path(spec: Pathfind): PathResult = {
    //Util.assert_eq(banned_nodes.contains(start), false)
    if (spec.banned_nodes.contains(spec.start)) {
      throw new IllegalArgumentException(s"A* from ${spec.start}, but ban ${spec.banned_nodes}")
    }
    Util.assert_eq(spec.banned_nodes.contains(spec.goal), false)
    if (spec.start == spec.goal && !spec.allow_cycles) {
      return PathResult(List(spec.start), Map(), 0)
    }

    // Stitch together our path
    val backrefs = new mutable.HashMap[Road, Road]()
    // We're finished with these
    val visited = new mutable.HashSet[Road]()
    // Best cost so far
    val costs = new mutable.HashMap[Road, (Double, Double)]()

    val open = new JavaPriorityQueue()
    val ordering_tuple = Ordering[(Double, Double)].on((pair: (Double, Double)) => pair)

    costs(spec.start) = spec.cost_start
    open.insert(spec.start, spec.calc_heuristic(spec.start))
    // Indicate start in backrefs by not including it

    while (open.nonEmpty) {
      val current = open.shift()
      if (!spec.allow_cycles || current != spec.start) {
        visited += current
      }

      // If backrefs doesn't have goal, allow_cycles is true and we just started
      if (current == spec.goal && backrefs.contains(spec.goal)) {
        // Reconstruct the path
        val path = new mutable.ListBuffer[Road]()
        var pointer: Option[Road] = Some(current)
        while (pointer.isDefined) {
          path.prepend(pointer.get)
          // Clean as we go to break loops
          pointer = backrefs.remove(pointer.get)
        }
        // Include 'start'
        if (spec.return_costs) {
          return PathResult(path.toList, costs.toMap, visited.size)
        } else {
          return PathResult(path.toList, Map[Road, (Double, Double)](), visited.size)
        }
      } else {
        // This foreach manually rewritten as a while to avoid closures in a tight loop.
        val succs = spec.successors(current)
        var i = 0
        while (i < succs.size) {
          val next_state = succs(i)
          i += 1
          if (!spec.banned_nodes.contains(next_state) && !visited.contains(next_state)) {
            val tentative_cost = spec.add_cost(
              costs(current), spec.calc_cost(current, next_state, costs(current))
            )
            if (!open.contains(next_state) || ordering_tuple.lt(tentative_cost, costs(next_state))) {
              backrefs(next_state) = current
              costs(next_state) = tentative_cost
              // if they're in open_members, modify weight in the queue? or
              // new step will clobber it. fine.
              open.insert(next_state, spec.add_cost(tentative_cost, spec.calc_heuristic(next_state)))
            }
          }
        }
      }
    }

    throw new PathfindingFailedException(s"Couldn't A* from ${spec.start} to ${spec.goal}")
  }
}

abstract class PriorityQueue() {
  def insert(item: Road, weight: (Double, Double))
  def shift(): Road
  def contains(item: Road): Boolean
  def nonEmpty(): Boolean

  // TODO have a change_weight.
}

// TODO generalize score.
class ScalaPriorityQueue() extends PriorityQueue {
  private case class Item(item: Road, weight: (Double, Double))

  private val pq = new mutable.PriorityQueue[Item]()(
    Ordering[(Double, Double)].on((item: Item) => item.weight).reverse
  )
  private val members = new mutable.HashSet[Road]()

  override def insert(item: Road, weight: (Double, Double)) {
    pq.enqueue(Item(item, weight))
    members += item
  }

  override def shift(): Road = {
    val item = pq.dequeue().item
    members -= item // TODO not true if there are multiples!
    return item
  }

  override def contains(item: Road) = members.contains(item)
  override def nonEmpty = pq.nonEmpty
}

class JavaPriorityQueue() extends PriorityQueue {
  private case class Item(item: Road, weight_1: Double, weight_2: Double)

  private val pq = new util.PriorityQueue[Item](100, new util.Comparator[Item]() {
    override def compare(a: Item, b: Item) =
      if (a.weight_1 < b.weight_1)
        -1
      else if (a.weight_1 > b.weight_1)
        1
      else if (a.weight_2 < b.weight_2)
        -1
      else if (a.weight_2 > b.weight_2)
        1
      else
        0
  })
  private val members = new mutable.HashSet[Road]()

  override def insert(item: Road, weight: (Double, Double)) {
    pq.add(Item(item, weight._1, weight._2))
    members += item
  }

  override def shift(): Road = {
    val item = pq.poll().item
    members -= item // TODO not true if there are multiples!
    return item
  }

  override def contains(item: Road) = members.contains(item)
  override def nonEmpty = !pq.isEmpty
}
