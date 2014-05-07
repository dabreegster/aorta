// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable

import utexas.aorta.map.{Edge, Vertex, Turn, Road}

import utexas.aorta.common.{Util, cfg, TurnID, RoadID}

class Pass3_Part2(graph: PreGraph3) {
  val roads_per_vert = new mutable.HashMap[Vertex, mutable.Set[Road]] with mutable.MultiMap[Vertex, Road]
  var turn_cnt = -1

  def run() {
    Util.log("Connecting the dots...")
    for (r <- graph.roads) {
      roads_per_vert.addBinding(r.v1, r)
      roads_per_vert.addBinding(r.v2, r)
    }
    Util.log_push()
    // TODO return the mapping in the future?
    for (v <- graph.vertices) {
      assert(roads_per_vert.contains(v))
      connect_vertex(v, roads_per_vert(v))
    }
    Util.log_pop()
  }

  private def next_id(): Int = {
    turn_cnt += 1
    return turn_cnt
  }

  // fill out an intersection with turns
  private def connect_vertex(v: Vertex, roads: mutable.Set[Road]) {
    // TODO return the turns or something so we can more efficiently set them
    // TODO cfg
    val cross_thresshold = math.Pi / 10 // allow 18 degrees

    def make_turn(pair: (Edge, Edge)) = new Turn(new TurnID(next_id), pair._1, pair._2)

    // To account for one-ways, we actually want to reason about roads that are
    // incoming to or outgoing from this vert.
    // Sorting is for determinism.
    val incoming_roads = roads.filter(_.incoming_lanes(v).nonEmpty).toList.sortBy(_.id.int)
    val outgoing_roads = roads.filter(_.outgoing_lanes(v).nonEmpty).toList.sortBy(_.id.int)

    // Only have to test one side
    def bad_uturn(r1: Road, r2: Road) = graph.other_side.get(r1) match {
      // Only allow this if this intersection only has these two roads
      case Some(other) if other == r2 => !(incoming_roads.size == 1 && outgoing_roads.size == 1)
      case _ => false
    }

    // this is a Cartesian product.
    for (r1 <- incoming_roads; r2 <- outgoing_roads if r1 != r2 && !bad_uturn(r1, r2)) {
      val from_edges = r1.incoming_lanes(v)
      val to_edges = r2.outgoing_lanes(v)

      // choose arbitrary representatives so we can make queries
      val from_rep = from_edges.head
      val to_rep   = to_edges.head

      // we want the angle to go from any 'from' edge to any 'to' edge
      val from_angle = from_rep.last_road_line.angle
      val to_angle = to_rep.first_road_line.angle

      // smallest angle of rotation, from "Agony" on gamedev TODO cite
      val angle_btwn = ((from_angle - to_angle + 3 * (math.Pi)) % (2 * math.Pi)) - math.Pi

      if (r1.osm_id == r2.osm_id || math.abs(angle_btwn) <= cross_thresshold) {
        // a crossing!
        // essentially zip the from's to the to's, but handle merging:
        // x -> x + n, make the 1 leftmost  lead to the n leftmost
        // x + n -> x, make the n rightmost lead to the 1 rightmost

        // TODO these rules are hard to generalize. when should we have
        // left/right-turn only lanes and stuff?

        val lane_diff = to_edges.length - from_edges.length

        if (lane_diff == 0) {
          // exact 1:1 mapping
          v.turns ++= from_edges.zip(to_edges).map(make_turn)
        } else if (lane_diff < 0) {
          // more to less. the rightmost will all have to merge.
          // we have 'to_edges.length - 1' regular dsts.
          val (mergers, regulars) = from_edges.splitAt(from_edges.length - (to_edges.length - 1))
          Util.assert_eq(regulars.length, to_edges.length - 1)

          v.turns ++= mergers.map(from => make_turn((from, to_edges.head)))
          v.turns ++= regulars.zip(to_edges.tail).map(make_turn)
        } else if (lane_diff > 0) {
          // less to more. the leftmost gets to pick many destinations.
          val lucky_src = from_edges.last
          val regular_srcs = from_edges.dropRight(1)

          val (regular_dsts, choices) = to_edges.splitAt(to_edges.size - lane_diff - 1)
          Util.assert_eq(regular_srcs.size, regular_dsts.size)
          
          v.turns ++= regular_srcs.zip(regular_dsts).map(make_turn)
          v.turns ++= choices.map(to => make_turn((lucky_src, to)))
        }
      } else if (angle_btwn < 0) {
        // no multiple turn lanes supported yet. it's just too hard to know when
        // this is the case.
        v.turns = make_turn((from_rep.leftmost_lane, to_rep.leftmost_lane)) :: v.turns
      } else {
        v.turns = make_turn((from_rep.rightmost_lane, to_rep.rightmost_lane)) :: v.turns
      }
    }
  }
}
