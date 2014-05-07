// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable

import utexas.aorta.map.{Coordinate, Road}

import utexas.aorta.common.Util

class PreGraph2(old_graph: PreGraph1) {
  // (v1, v2, road name) to the edge. used for detecting cul-de-sacs easily.
  private val edge_lookup = new mutable.HashMap[(Coordinate, Coordinate, String), PreEdge2]

  // find true edges between adjacent vertices
  Util.log("Splitting " + old_graph.edges.length + " roads into edges between intersections")
  var edges = old_graph.edges.flatMap(split_road)

  def split_road(road: PreEdge1): List[PreEdge2] = {
    // Walk the list of points in this edge, discovering chains between
    // vertices

    // TODO this is essentially a 'split at vertices'
    val split_edges = new mutable.ListBuffer[Option[PreEdge2]]

    //Util.assert_eq(old_graph.is_vert(road.points.head), true)
    if (!old_graph.is_vert(road.points.head)) {
      return Nil
    }

    var start = 0
    // List.range is [lower, upper)
    for (i <- List.range(0, road.points.length)) {
      if (start != i && old_graph.is_vert(road.points(i))) {
        // so we have an edge from start to i
        // slice is [from, till), hence the +1
        split_edges += find_or_make_edge(road.points.slice(start, i + 1), road)
        start = i
      }
    }
    //Util.assert_eq(start, road.points.length - 1);
    // TODO seeing this break on new maps from OSM, no time to investigate right
    // now
    if (start != road.points.length - 1) {
      return Nil
    }

    // maybe Nil, if so, flatMap doesn't care
    // honey badger doesn't give a fuck
    return split_edges.toList.flatten
  }

  // None if it's already there
  def find_or_make_edge(points: List[Coordinate], edge_dat: PreEdge1): Option[PreEdge2] =
  {
    val v1 = points.head
    val v2 = points.last

    // do we already have an edge from v1->v2 or v2->v1?
    // this handling mainly needs to deal with cul-de-sacs

    return if (edge_lookup.contains((v1, v2, edge_dat.name)) ||
               edge_lookup.contains((v2, v1, edge_dat.name)))
    {
      //Util.log("well, finally! already exists " + edge_dat.name)
      // TODO make a new edge if the points dont match?
      None
    } else {
      val e = new PreEdge2(v1, v2, points, edge_dat)
      //edge_lookup((v1, v2, edge_dat.name)) = e
      Some(e)
    }
  }
}

class PreEdge2(var from: Coordinate, var to: Coordinate,
               val points: List[Coordinate], val dat: PreEdge1)
  extends Ordered[PreEdge2]
{
  val id = PreEdge2.next_id
  PreEdge2.next_id += 1
  override def compare(other: PreEdge2) = id.compare(other.id)
  override def toString = s"Road ${dat.name} btwn $from and $to"

  def length = Road.road_len(points)
  def is_culdesac = from == to
}

object PreEdge2 {
  var next_id = 0
}
