// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.common.{StateReader, TurnID, EdgeID}

class Turn(val id: TurnID, val from: Edge, val to: Edge)
  extends Traversable(Array(new Line(from.end_pt, to.start_pt))) with Ordered[Turn]
{
  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: MapStateWriter) {
    w.int(id.int)
    w.int(w.edges(from.id).int)
    w.int(w.edges(to.id).int)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def compare(other: Turn) = id.int.compare(other.id.int)
  override def asTurn = this
  override def asEdge = throw new Exception("This is a turn, not an edge")

  override def toString = "turn[" + id + "](" + from + ", " + to + ")"
  // Short form is nice.
  //override def toString = "Turn(" + from.id + ", " + to.id + ")"

  def leads_to = List(to)
  def speed_limit = to.speed_limit

  def vert = from.to

  def conflict_line = lines.head
  def conflicts_with(t: Turn) =
    (from != t.from) && (to == t.to || conflict_line.segment_intersection(t.conflict_line).isDefined)

  // TODO more efficiently?
  def conflicts = vert.turns.filter(conflicts_with).toSet

  def angle_deg = math.toDegrees(conflict_line.angle)
}

object Turn {
  def unserialize(r: StateReader, edges: Array[Edge]) = new Turn(
    new TurnID(r.int), edges(r.int), edges(r.int)
  )
}
