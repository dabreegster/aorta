// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable

import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.common.{Util, StateReader, VertexID, EdgeID, RoadID}

class Graph(
  val roads: Array[Road], val edges: Array[Edge], val vertices: Array[Vertex],
  val artifacts: Array[RoadArtifact], val width: Double, val height: Double, val offX: Double,
  val offY: Double, val scale: Double, val name: String
) {
  //////////////////////////////////////////////////////////////////////////////
  // Deterministic state

  // TODO if we squish down IDs, it can be an array too!
  val turns = vertices.foldLeft(List[Turn]())(
    (l, v) => v.turns.toList ++ l
  ).map(t => t.id -> t).toMap

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: MapStateWriter) {
    w.doubles(width, height, offX, offY, scale)
    w.int(roads.size)
    roads.foreach(r => r.serialize(w))
    w.int(edges.size)
    edges.foreach(e => e.serialize(w))
    w.int(vertices.size)
    vertices.foreach(v => v.serialize(w))
    w.string(name)
    w.int(artifacts.size)
    artifacts.foreach(a => a.serialize(w))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def traversables() = edges ++ turns.values

  def get_v(id: VertexID) = vertices(id.int)
  def get_e(id: EdgeID) = edges(id.int)
  def get_r(id: RoadID) = roads(id.int)

  // TODO file library
  def basename = name.replace("maps/", "").replace(".map", "")
}

// It's a bit funky, but the actual graph instance doesn't have this; we do.
// TODO This is the only remaining global mutable state singleton remaining as of May 2014, and
// there's a proper fix that's hard. For now, leave it, it's fine, just can't simultaneously
// simulate unless both sims use the same map.
object Graph {
  var width = 0.0
  var height = 0.0
  var xoff = 0.0
  var yoff = 0.0
  var scale = 0.0

  private val cached_graphs = new mutable.HashMap[String, Graph]()

  // this MUST be set before world_to_gps is called.
  // TODO get rid of this approach once GPS coordinates always retained
  def set_params(w: Double, h: Double, x: Double, y: Double, s: Double) {
    width = w
    height = h
    xoff = x
    yoff = y
    scale = s
  }

  // inverts what PreGraph1's normalize() does.
  def world_to_gps(x: Double, y: Double) = Coordinate(
    (x / scale) - xoff, ((height - y) / scale) - yoff
  )

  // TODO traversables have Queues and vertices have Intersections for speed
  // Set fresh_copy to true to force a new version of everything, otherwise caching'll return the
  // same copy
  var fresh_copy = false
  def load(fn: String): Graph = {
    if (fresh_copy || !cached_graphs.contains(fn)) {
      print(s"Loading $fn...")
      cached_graphs(fn) = unserialize(Util.reader(fn))
      println(s"\rLoaded $fn.     ")
    }
    return cached_graphs(fn)
  }

  def unserialize(r: StateReader): Graph = {
    // Set these before loading any traversables, since length'll be computed from em
    val w = r.double
    val h = r.double
    val xo = r.double
    val yo = r.double
    val s = r.double
    set_params(w, h, xo, yo, s)
    val roads = Range(0, r.int).map(_ => Road.unserialize(r)).toArray
    val edges = Range(0, r.int).map(_ => Edge.unserialize(r, roads)).toArray
    val vertices = Range(0, r.int).map(_ => Vertex.unserialize(r, edges)).toArray
    val name = r.string
    val artifacts = Range(0, r.int).map(_ => RoadArtifact.unserialize(r)).toArray
    val g = new Graph(roads, edges, vertices, artifacts, w, h, xo, yo, s, name)
    // Dependency between roads, edges, and vertices is cyclic, so have to set up one of these.
    g.roads.foreach(r => r.setup(vertices))
    return g
  }
}
