// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

// TODO I don't want this dependency, but at the moment, it leads to a great
// perf boost due to dropping a pricy hash lookup
import utexas.aorta.sim.Queue
import utexas.aorta.ui.Renderable

import utexas.aorta.common.{cfg, Util, Physics, StateWriter, StateReader, TurnID, Serializable}

// Something with a sequence of lines forming a path and a way to get to more
// somethings
abstract class Traversable(val lines: Array[Line]) {
  //////////////////////////////////////////////////////////////////////////////
  // Deterministic state

  // Store; it's not free to compute it constantly
  val length: Double = lines.foldLeft(0.0)((a, b) => a + b.length)
  this match {
    case e: Edge if length <= cfg.epsilon => {
      // Really the limit should be end_threshold, but a few crop up from time to time...
      Util.log(s"Lane ${e.id} has short length $length!")
    }
    case _ =>
  }

  // TODO temporary perf fix
  var queue: Queue = null

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def leads_to: List[Traversable]
  def speed_limit: Double

  // Sometimes we statically know if a traversable should be a turn or edge and want something less
  // verbose than pattern matching
  def asEdge: Edge
  def asTurn: Turn

  def cost = length / speed_limit

  // if dist is > length or < 0, then this query makes no sense
  def location(dist: Double) = current_pos(dist) match {
    case (l: Line, dist_along: Double) => l.point_on(dist_along)
  }

  // Find a point along one our lines that's close the given point
  // TODO this is inefficient!
  def approx_dist(pt: Coordinate, step_size: Double): Double =
    (0.0 until length by step_size).map(
      dist => (location(dist).dist_to(pt), dist)
    ).min._2

  // returns line and distance along that line
  def current_pos(dist: Double): (Line, Double) = {
    if (dist < 0) {
      throw new Exception("Negative distance on a location?!")
    }

    // TODO it's late, I am not going to write this functionally...
    var at = dist
    for (l <- lines) {
      if (at > l.length) {
        at -= l.length
      } else {
        // TODO dont return inside here. fold?
        return (l, at)
      }
    }

    throw new Exception(s"$dist is too long for $this (length $length)")
  }

  def start_pt = lines.head.start
  def end_pt  = lines.last.end
  def approx_midpt = lines(lines.size / 2).midpt

  def worst_entry_dist = Physics.worst_entry_dist(speed_limit)
}

class Line(val x1: Double, val y1: Double, val x2: Double, val y2: Double) {
  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.doubles(x1, y1, x2, y2)
  }

  def this(pt1: Coordinate, pt2: Coordinate) = this(pt1.x, pt1.y, pt2.x, pt2.y)
  def this(v1: Vertex, v2: Vertex) = this(v1.location, v2.location)

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  // TODO Compute and store it once, since the math isn't free?
  def length = Coordinate.gps_dist_in_meters(
    Graph.world_to_gps(x1, y1), Graph.world_to_gps(x2, y2)
  )

  // return [0, 2pi) like a reasonable bloody...
  // also, this is a place where we have to recall the coordinate system has y
  // increasing down, but trig thinks y increases up...
  def angle: Double = {
    val theta = math.atan2(y1 - y2, x2 - x1)  // y inversion
    return if (theta < 0)
      theta + (2 * math.Pi)
    else
      theta
  }

  // TODO shiftline() broke with the seemingly correct angle(). testing.
  def broken_angle = math.atan2(y2 - y1, x2 - x1)

  def midpt = Coordinate((x1 + x2) / 2, (y1 + y2) / 2)
  def width = x2 - x1
  def height = y2 - y1
  def start = Coordinate(x1, y1)
  def end = Coordinate(x2, y2)

  override def toString = s"($x1, $y1) ---> ($x2, $y2)"

  def segment_intersection(other: Line): Option[Coordinate] = {
    // Ripped from http://paulbourke.net/geometry/pointlineplane/
    // and http://www.java-gaming.org/index.php?topic=22590.0
    def det(a: Double, b: Double, c: Double, d: Double) = (a * d) - (b * c)

    val x3 = other.x1
    val x4 = other.x2
    val y3 = other.y1
    val y4 = other.y2

    val detDiff = det(x1 - x2, y1 - y2, x3 - x4, y3 - y4)
    return if (detDiff.abs <= cfg.epsilon) {
      None  // parallel
    } else {
      // Do the segments intersect?
      val numer_a = (x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)
      val numer_b = (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
      val a = numer_a / detDiff
      val b = numer_b / detDiff
      if (a < 0 || a > 1 || b < 0 || b > 1) {
        return None
      } else {
        val det1And2 = det(x1, y1, x2, y2)
        val det3And4 = det(x3, y3, x4, y4)
        val x = det(det1And2, x1 - x2, det3And4, x3 - x4) / detDiff
        val y = det(det1And2, y1 - y2, det3And4, y3 - y4) / detDiff
        return Some(Coordinate(x, y))
      }
    }
  }

  // where are we on this line? even handles negative distances
  def point_on(dist_along: Double): Coordinate = {
    val percent = dist_along / length
    return Coordinate(x1 + (width * percent), y1 + (height * percent))
  }

  def perp_shift(off: Double): Line = {
    // just move in the direction of the road (as given by the ordering of the
    // points) plus 90 degrees clockwise
    // TODO why does the angle() that respects inversion fail?
    val theta = broken_angle + (math.Pi / 2)
    val dx = off * cfg.lane_width * math.cos(theta)
    val dy = off * cfg.lane_width * math.sin(theta)

    return new Line(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
  }

  // When percent = 0, returns this. When percent = 1, return l. Otherwise,
  // interpolates between the two lines.
  def add_scaled(l: Line, percent: Double) = new Line(
    x1 + ((l.x1 - x1) * percent), y1 + ((l.y1 - y1) * percent),
    x2 + ((l.x2 - x2) * percent), y2 + ((l.y2 - y2) * percent)
  )

  // this takes a point along a line and moves it back
  // TODO y inversion problems still?!
  private def shift_pt(x: Double, y: Double, theta: Double, mag: Double) = Coordinate(
    x + (mag * math.cos(theta)), y - (mag * math.sin(theta))
  )
  // TODO cfg for shift_mag
  def shift_fwd(mag: Double = 1.5) = shift_pt(x1, y1, angle, mag)
  def shift_back(mag: Double = 1.5) = shift_pt(x2, y2, angle + math.Pi, mag)
}

object Line {
  def unserialize(r: StateReader) =
    new Line(r.double, r.double, r.double, r.double)
}

case class Position(on: Traversable, dist: Double) extends Renderable with Serializable {
  Util.assert_ge(dist, 0)
  Util.assert_le(dist, on.length)

  def location = on.location(dist)
  def dist_left = on.length - dist
  override def toString = s"($on, $dist)"
  override def tooltip = List(f"$on at $dist%.2f")

  def debug = {
    Util.log(toString)
    on match {
      case e: Edge => e.debug
      case _ =>
    }
  }

  def serialize(w: StateWriter) {
    on match {
      case e: Edge => {
        w.bool(true)
        w.int(e.id.int)
      }
      case t: Turn => {
        w.bool(false)
        w.int(t.id.int)
      }
    }
    w.double(dist)
  }
}

object Position {
  def unserialize(r: StateReader, graph: Graph): Position = {
    val on: Traversable =
      if (r.bool)
        graph.edges(r.int)
      else
        graph.turns(new TurnID(r.int))
    return Position(on, r.double)
  }
}
