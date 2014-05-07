// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import Function.tupled

import utexas.aorta.ui.Renderable
import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.common.{Util, StateReader, VertexID}

// TODO I don't want this dependency, but at the moment, it leads to a great
// perf boost due to dropping a pricy hash lookup
import utexas.aorta.sim.intersections.Intersection

class Vertex(val location: Coordinate, val id: VertexID) extends Renderable {
  //////////////////////////////////////////////////////////////////////////////
  // State

  // TODO we could keep a map for faster lookup
  var turns: List[Turn] = Nil

  //////////////////////////////////////////////////////////////////////////////
  // Deterministic state

  // TODO messy to have this dependency here.
  var intersection: Intersection = null

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: MapStateWriter) {
    location.serialize(w)
    w.int(w.vertices(id).int)
    w.int(turns.length)
    turns.foreach(t => t.serialize(w))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def turns_from(from: Edge): List[Turn] = turns.filter(_.from == from)
  def turns_to(to: Edge): List[Turn] = turns.filter(_.to == to)
  def edges_to(to: Road): List[Edge] =
    turns.filter(_.to.road == to).map(_.from)

  // what verts lead to this one?
  def in_verts = turns.map(t => t.from.from).toSet
  // what verts does this one lead to?
  def out_verts = turns.map(t => t.to.to).toSet

  def roads_in = turns.map(_.from.road).toSet
  // A bit of a hack, using lane->lane turns
  def road_conflicts(from: Road, to: Road) =
    turns.find(t => t.from.road == from && t.to.road == to).get
    .conflicts.map(t => (t.from.road, t.to.road))
  def edges = turns.flatMap(t => List(t.from, t.to))
  def roads = edges.map(_.road).toSet
  def in_edges = turns.map(t => t.from).toSet
  def out_edges = turns.map(t => t.to).toSet

  override def toString = "[V" + id + "]"

  override def equals(other: Any) = other match {
    case other: Vertex => { id == other.id }
    case _ => false
  }

  def debug = {
    Util.log(this + " at " + location)

    val i = intersection

    Util.log("Current turns allowed:")
    Util.log_push
    i.policy.current_greens.foreach(g => Util.log("" + g))
    Util.log_pop
                                                                        
    Util.log("Current turns active:")                                   
    Util.log_push
    i.turns.foreach(tupled((turn, count) => Util.log(s"$count doing $turn")))
    Util.log_pop

    Util.log("Roads: " + roads)

    // anything else
    i.policy.dump_info
  }
}

object Vertex {
  def unserialize(r: StateReader, edges: Array[Edge]): Vertex = {
    val v = new Vertex(Coordinate.unserialize(r), new VertexID(r.int))
    v.turns ++= Range(0, r.int).map(_ => Turn.unserialize(r, edges: Array[Edge]))
    return v
  }
}
