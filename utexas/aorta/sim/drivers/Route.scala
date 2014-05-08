// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import Function.tupled
import scala.collection.immutable
import scala.collection.mutable

import utexas.aorta.map.{Edge, Road, Traversable, Turn, Vertex, Graph, Router}
import utexas.aorta.sim.{EV_Transition, EV_Reroute}
import utexas.aorta.sim.make.{RouterType, ReroutePolicyType}

import utexas.aorta.common.{Util, cfg, StateWriter, StateReader, TurnID, Serializable}
import utexas.aorta.common.algorithms.{Pathfind, PathfindingFailedException}

// Get a client to their goal by any means possible.
abstract class Route(val goal: Road, reroute_policy_type: ReroutePolicyType.Value) extends Serializable {
  //////////////////////////////////////////////////////////////////////////////
  // Transient state

  protected var owner: Agent = null
  protected var debug_me = false  // TODO just grab from owner?
  protected var reroute_policy: ReroutePolicy = null // TODO is it transient?

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.ints(goal.id.int, reroute_policy_type.id)
  }

  protected def unserialize(r: StateReader, graph: Graph) {}

  def setup(a: Agent) {
    owner = a
    reroute_policy = ReroutePolicyType.make(reroute_policy_type, a)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def react() {
    reroute_policy.react()
  }

  // The client tells us they've physically moved
  def transition(from: Traversable, to: Traversable)
  // The client is being forced to pick a turn. If they ask us repeatedly, we
  // have to always return the same answer. Should have no side effects if query_only.
  def pick_turn(e: Edge, query_only: Boolean = false): Turn
  // Prescribe the final lane on this road to aim for. We should be able to spazz around in
  // our answer here. True if e doesn't already lead to next road, aka, the final lane is more than
  // recommended.
  def pick_final_lane(e: Edge): (Edge, Boolean)
  // May fail
  def optional_reroute(at: Edge) {}

  def set_debug(value: Boolean) {
    debug_me = value
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def done(at: Edge) = at.road == goal
  def dump_info()
  // Includes current road
  def current_path(): List[Road]
}

object Route {
  def unserialize(r: StateReader, graph: Graph): Route = {
    // Original router will never be used again, and rerouter will have to be reset by PathRoute.
    val route = new PathRoute(
      graph.roads(r.int), RouterType.make(RouterType.Fixed, graph, Nil),
      RouterType.make(RouterType.Fixed, graph, Nil), ReroutePolicyType(r.int)
    )
    route.unserialize(r, graph)
    return route
  }
}

// Follow routes prescribed by routers. Only reroute when forced or encouraged to.
// TODO rerouter only var due to serialization
class PathRoute(
  goal: Road, orig_router: Router, private var rerouter: Router, reroute_policy_type: ReroutePolicyType.Value
) extends Route(goal, reroute_policy_type) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  // Head is the current step.
  private var path: List[Road] = Nil

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    w.int(rerouter.router_type.id)
    // We can't tell when we last rerouted given less state; store the full path.
    w.list_int(path.map(_.id.int))
  }

  override def unserialize(r: StateReader, graph: Graph) {
    rerouter = RouterType.make(RouterType(r.int), graph, Nil)
    path = Range(0, r.int).map(_ => graph.roads(r.int)).toList
  }

  override def setup(a: Agent) {
    super.setup(a)
    orig_router.setup(a)
    rerouter.setup(a)

    Util.assert_eq(path.isEmpty, true)
    path = orig_router.path(a.at.on.asEdge.road, goal).path
    owner.sim.publish(
      EV_Reroute(owner, path, true, orig_router.router_type, false, Nil), owner
    )
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def transition(from: Traversable, to: Traversable) {
    (from, to) match {
      case (e: Edge, _: Turn) => {
        if (e.road == path.head) {
          path = path.tail
        } else {
          throw new Exception(s"Route not being followed! $from -> $to happened, with path $path")
        }
      }
      case _ =>
    }
    owner.sim.publish(EV_Transition(owner, from, to), owner)
  }

  def pick_turn(e: Edge, query_only: Boolean): Turn = {
    // Lookahead could be calling us from anywhere. Figure out where we are in the path.
    val (before, slice) = path.span(r => r != e.road)
    Util.assert_eq(slice.nonEmpty, true)
    val dest = slice.tail.head

    // Is the next step reachable?
    val must_reroute = e.next_turns.filter(t => t.to.road == dest).isEmpty
    // This variant only considers long roads capable of being congested, which is risky...
    // TODO make the client do this? yes! remove policy from mechanism.
    val should_reroute = dest.congested

    val turn = if (!query_only && (must_reroute || should_reroute)) {
      if (must_reroute) {
        mandatory_reroute(e, before)
      } else {
        optional_reroute(e)
      }

      val (_, rerouted_slice) = path.span(r => r != e.road)
      best_turn(e, rerouted_slice.tail.head, rerouted_slice.tail.tail.headOption.getOrElse(null))
    } else {
      best_turn(e, dest, slice.tail.tail.headOption.getOrElse(null))
    }
    return turn
  }

  def pick_final_lane(from: Edge): (Edge, Boolean) = {
    // We could be done!
    if (from.road == goal) {
      return (from, false)
    }

    // Lookahead could be calling us from anywhere. Figure out where we are in the path.
    val slice = path.dropWhile(r => r != from.road)
    Util.assert_eq(slice.nonEmpty, true)

    val next_step = slice.tail.head
    // Find all lanes going to the next step.
    val candidates = candidate_lanes(from, next_step) match {
      case Nil => throw new Exception(s"Other lanes around $from don't lead to $next_step!")
      case lanes => lanes
    }

    // Pick the lane closest to the current
    //return candidates.minBy(e => math.abs(from.lane_num - e.lane_num))

    // TODO lookahead a bit to lane-change early

    // Discretionary lane-changing: pick the lane with the fewest people ahead of us
    return (candidates.minBy(e => owner.num_ahead(e)), !candidates.contains(from))
  }

  // Ultimately, fuse mandatory and optional? Mandatory may produce longer paths because it starts
  // from an arbitrary choice, and it may loop back in on itself to try to LC. Optional avoids these
  // two issues, but it may fail.
  private def mandatory_reroute(at: Edge, slice_before: List[Road]) {
    Util.assert_ne(at.road, goal)
    val old_path = path
    path = slice_before ++ (at.road :: rerouter.path(Pathfind(
      // Start from a source we can definitely reach without lane-changing.
      start = at.next_turns.maxBy(t => t.to.queue.percent_avail).to.road,
      goal = goal
      // Don't ban any roads from the path. If we wind up looping back on something, then for now,
      // so be it.
    )).path)
    owner.sim.publish(
      EV_Reroute(owner, path, false, rerouter.router_type, true, old_path), owner
    )
  }

  override def optional_reroute(at: Edge) {
    Util.assert_ne(at.road, goal)
    // TODO assert at.road is in current path
    val slice_before = path.takeWhile(r => r != at.road)
    val old_path = path
    try {
      val new_path = slice_before ++ rerouter.path(Pathfind(
          start = at.road,
          goal = goal,
          // Don't hit anything already in our path
          banned_nodes = slice_before.toSet
          // Let the algorithm pick the best next step
        ).first_succs(at.next_roads.toArray)
      ).path
      if (reroute_policy.approve_reroute(old_path, new_path)) {
        path = new_path
        owner.sim.publish(
          EV_Reroute(owner, path, false, rerouter.router_type, false, old_path), owner
        )
      }
    } catch {
      // Couldn't A* due to constraints, but that's alright
      case e: PathfindingFailedException =>
    }
  }

  override def set_debug(value: Boolean) {
    debug_me = value
    orig_router.set_debug(value)
    rerouter.set_debug(value)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def dump_info() {
    Util.log(s"Path route to $goal using $path")
  }

  def current_path = path
  def next_roads(num: Int) = path.take(num)

  // Prefer the one that's emptiest now and try to get close to a lane that
  // we'll want to LC to anyway. Only call when we haven't chosen something yet.
  private def best_turn(e: Edge, dest: Road, next_dest: Road): Turn = {
    val ideal_lanes =
      if (next_dest != null)
        candidate_lanes(dest.rightmost, next_dest)
      else
        dest.lanes.toList
    def ideal_dist(e: Edge) =
      ideal_lanes.map(ideal => math.abs(e.lane_num - ideal.lane_num)).min
    val total = dest.lanes.size
    def ranking(t: Turn) =
      if (t.to.road != dest)
        -1
      else
        // How far is this lane from an ideal lane?
        (total - ideal_dist(t.to)).toDouble / total.toDouble

    // Prefer things close to where we'll need to go next, and things with more
    // room.
    return e.next_turns.maxBy(
      t => (ranking(t), t.to.queue.percent_avail)
    )
  }

  private def candidate_lanes(from: Edge, dest: Road) =
    from.other_lanes.filter(f => f.succs.exists(t => t.road == dest)).toList
}
