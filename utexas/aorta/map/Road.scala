// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable
import Function.tupled

import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.ui.Renderable
import utexas.aorta.common.{Util, RoadID, VertexID, Physics, StateReader}

// An oriented bundle of lanes
class Road(
  val id: RoadID, val dir: Direction.Value, val length: Double, val name: String,
  val road_type: String, val osm_id: String, v1_id: VertexID, v2_id: VertexID,
  val points: Array[Coordinate]
) extends Ordered[Road] with Renderable
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  var v1: Vertex = null
  var v2: Vertex = null
  val lanes = new mutable.ListBuffer[Edge]()

  // TODO lets figure out how to build immutable stuff.
  val houses = new mutable.ListBuffer[Coordinate]()
  val shops = new mutable.ListBuffer[Coordinate]()

  //////////////////////////////////////////////////////////////////////////////
  // Deterministic state

  // TODO move this table. actually, store speed limit
  val speed_limit = Physics.mph_to_si(road_type match {
    case "residential"    => 30
    case "motorway"       => 80
    // Actually these don't have a speed limit legally...  35 is suggested, but NOBODY does that
    case "motorway_link"  => 70
    case "trunk"          => 70
    case "trunk_link"     => 60
    case "primary"        => 65
    case "primary_link"   => 55
    case "secondary"      => 55
    case "secondary_link" => 45
    case "tertiary"       => 45
    case "tertiary_link"  => 35
    //case "unclassified"   => 40
    //case "road"           => 40
    case "living_street"  => 20
    // TODO some of these we filter out in Pass 1... cross-ref with that list
    case "service"        => 10 // This is apparently parking-lots basically, not feeder roads
    case "services"       => 10
    //case "track"          => 35
    // I feel the need.  The need for speed.  Where can we find one of these?
    //case "raceway"        => 300
    //case "null"           => 30
    //case "proposed"       => 35
    //case "construction"     => 20

    case _                => 35 // Generally a safe speed, right?
  })

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: MapStateWriter) {
    w.int(w.roads(id).int)
    w.int(dir.id)
    w.double(length)
    w.strings(name, road_type, osm_id)
    w.int(w.vertices(v1.id).int)
    w.int(w.vertices(v2.id).int)
    w.int(points.size)
    points.foreach(pt => pt.serialize(w))
    w.int(shops.size)
    shops.foreach(pt => pt.serialize(w))
    w.int(houses.size)
    houses.foreach(pt => pt.serialize(w))
  }

  def setup(vertices: Array[Vertex]) {
    v1 = vertices(v1_id.int)
    v2 = vertices(v2_id.int)

    // check invariants of points -- oops, not true anymore since we merge short
    // roads
    //Util.assert_eq(v1.location, points.head)
    //Util.assert_eq(v2.location, points.last)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = s"$name's $dir lanes ($id)"
  override def compare(other: Road) = id.int.compare(other.id.int)

  def incoming_lanes(v: Vertex) = if (v == v2) lanes else Nil
  def outgoing_lanes(v: Vertex) = if (v == v1) lanes else Nil

  def rightmost = lanes.head

  def from = v1
  def to = v2

  def start_pt = rightmost.from.location
  def end_pt = rightmost.to.location
  // TODO dont assume some edge being lane-changeable means others are too
  // TODO could even predict/take into account the current distance to see if
  // there's room left
  def naive_leads_to = lanes.flatMap(_.succs).map(_.road).toSet
  def leads_to(from: Edge) = if (from.ok_to_lanechange)
                               naive_leads_to
                             else
                               from.succs.map(_.road).toSet

  def freeflow_time = lanes.map(_.length).min / speed_limit

  lazy val succs = lanes.flatMap(e => e.next_turns.map(t => t.to.road)).toArray
  def preds = lanes.flatMap(e => e.prev_turns.map(t => t.from.road))
  def next_roads = lanes.flatMap(e => e.next_roads).toSet

  def lines = points.zip(points.tail).map(p => shift_line(p))

  // For debug only
  def doomed = lanes.exists(e => e.doomed)

  def num_lanes = lanes.size

  override def debug() {
    Util.log(this + " is a " + road_type + " of length " + length + " meters")
    Util.log(s"  Originally OSM id = $osm_id")
  }

  private def shift_line(pair: (Coordinate, Coordinate)) =
    new Line(pair._1, pair._2).perp_shift(num_lanes / 2.0)

  // TODO better heuristic, based on how much this extended road touches other
  // roads
  def is_major = road_type != "residential"

  // TODO the use of these is deprecated at best
  def freeflow_percent_full = lanes.map(_.queue.percent_freeflow_full).max
  def congested = lanes.exists(e => e.queue.is_congested)
}

object Road {
  def unserialize(r: StateReader): Road = {
    val road = new Road(
      new RoadID(r.int), Direction(r.int), r.double, r.string, r.string, r.string,
      new VertexID(r.int), new VertexID(r.int),
      Range(0, r.int).map(_ => Coordinate.unserialize(r)).toArray
    )
    Range(0, r.int).foreach(_ => road.shops += Coordinate.unserialize(r))
    Range(0, r.int).foreach(_ => road.houses += Coordinate.unserialize(r))
    return road
  }

  def road_len(pts: Iterable[Coordinate]) =
    pts.zip(pts.tail).map(tupled((p1, p2) => new Line(p1, p2).length)).sum
}

// When we merge short roads, we get rid of geometry. Preserve it here for the GUI's sake in
// absolutely minimal form.
case class RoadArtifact(points: Array[Coordinate]) {
  def serialize(w: MapStateWriter) {
    w.int(points.size)
    points.foreach(pt => pt.serialize(w))
  }

  def lines = points.zip(points.tail).map(p => shift_line(p))
  private def shift_line(pair: (Coordinate, Coordinate)) =
    // this shift helps center the drawn artifact
    new Line(pair._1, pair._2).perp_shift(-0.25)
}

object RoadArtifact {
  def unserialize(r: StateReader) = RoadArtifact(
    Range(0, r.int).map(_ => Coordinate.unserialize(r)).toArray
  )
}
