// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable
import utexas.aorta.ui.Renderable
import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.common.{cfg, RNG, Util, StateReader, EdgeID, RoadID, Price}

// TODO var lane num due to fixing IDs. necessary?
class Edge(
  val id: EdgeID, val road: Road, var lane_num: Int, geometry: Array[Line]
) extends Traversable(geometry) with Renderable with Ordered[Edge]
{
  road.lanes += this

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: MapStateWriter) {
    w.int(w.edges(id).int)
    w.int(w.roads(road.id).int)
    w.int(lane_num)
    w.int(lines.length)
    w.first_lines.getOrElse(this, lines.head).serialize(w)
    lines.tail.dropRight(1).foreach(l => l.serialize(w))
    if (lines.size > 1) {
      w.last_lines.getOrElse(this, lines.last).serialize(w)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def compare(other: Edge) = id.int.compare(other.id.int)
  // TODO arbitrary turn for toll
  override def toString = s"Lane ${road.dir}${lane_num} of ${road.name} (E$id, R${road.id}) (toll ${to.intersection.tollbooth.toll(next_turns.head)})"
  override def asEdge = this
  override def asTurn = throw new Exception("This is an edge, not a turn")

  // no lane-changing
  //def leads_to = next_turns
  // with lane-changing
  def leads_to = next_turns ++ List(shift_left, shift_right).flatten
  def speed_limit = road.speed_limit

  def turns_leading_to(group: Road) =
    next_turns.filter(t => t.to.road == group)

  def other_lanes = road.lanes
  def rightmost_lane = other_lanes.head
  def leftmost_lane  = other_lanes.last

  def shift_left: Option[Edge]  = if (is_leftmost)  None else Some(other_lanes(lane_num + 1))
  def shift_right: Option[Edge] = if (is_rightmost) None else Some(other_lanes(lane_num - 1))
  def adjacent_lanes: List[Edge] = List(shift_left, shift_right, Some(this)).flatten
  def best_adj_lane(to_reach: Edge)
    = adjacent_lanes.minBy(e => math.abs(to_reach.lane_num - e.lane_num))

  def next_turns = to.turns_from(this)
  def prev_turns = from.turns_to(this)
  def succs: List[Edge] = next_turns.map(t => t.to)
  def preds: List[Edge] = prev_turns.map(t => t.from)

  def next_roads = next_turns.map(t => t.to.road)

  def is_rightmost = lane_num == 0
  def is_leftmost  = lane_num == other_lanes.size - 1

  // not for one-ways right now. but TODO it'd be cool to put that here.
  def lane_offset = other_lanes.length - lane_num

  def from = road.from
  def to = road.to

  //////// Geometry. TODO separate somewhere?

  // recall + means v1->v2, and that road's points are stored in that order
  // what's the first line segment we traverse following this lane?
  def first_road_line = road.lines.head
  // what's the last line segment we traverse following this lane?
  def last_road_line = road.lines.last

  def debug = {
    Util.log(this + " has length " + length + " m, min entry dist " +
             (worst_entry_dist + cfg.follow_dist))
    Util.log("(lanechange dist is " + (cfg.lanechange_dist +
             cfg.end_threshold) + ")")
    Util.log("Queue contains " + queue.agents)
    Util.log(s"Speed lim $speed_limit, still capacity ${queue.capacity}, freeflow capacity ${queue.freeflow_capacity}")
    Util.log("Succs: " + next_turns)
    Util.log("Preds: " + prev_turns)
    Util.log(s"From $from to $to")
    Util.log(s"${road.houses.size} houses, ${road.shops.size} shops")
  }

  // For debug only
  def doomed = next_turns.isEmpty || prev_turns.isEmpty

  // TODO Starting on highways or in the middle lane seems weird, but allow it for now
  // TODO justify this better, or cite the paper.
  def ok_to_spawn = length >= worst_entry_dist + cfg.end_threshold + (2 * cfg.follow_dist)

  // TODO geometric argument
  // TODO sometimes the max arg is < the min arg. :)
  def safe_spawn_dist(rng: RNG) = rng.double(
    worst_entry_dist + cfg.follow_dist, length - cfg.end_threshold
  )

  def ok_to_lanechange = length >= cfg.lanechange_dist + cfg.end_threshold

  // If true, somebody's turning into this lane and already has a turn secured.
  def dont_block() = from.intersection.policy.approveds_to(this).exists(
    t => t.a.wont_block(from.intersection)
  )
}

object Edge {
  def unserialize(r: StateReader, roads: Array[Road]) = new Edge(
    new EdgeID(r.int), roads(r.int), r.int, Range(0, r.int).map(_ => Line.unserialize(r)).toArray
  )
}

// This is completely arbitrary, it doesn't really mean anything
object Direction extends Enumeration {
  type Direction = Value
  val POS = Value("+")
  val NEG = Value("-")
}
