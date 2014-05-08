// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.{mutable, immutable}
import Function.tupled

import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.sim.make.RouterType
import utexas.aorta.ui.Renderable
import utexas.aorta.common.algorithms.{AStar, Pathfind}
import utexas.aorta.common.{Util, ZoneID, StateReader}

// TODO move to contrib until stable (but map serializes zones)

// TODO consider dropping the requirement that zones have to be connected. allows for more flexible
// shapes, and for disjoint partitioning.

// TODO something with zones isnt deterministic.

// TODO RoadAgent and related at the zone granularity?

class ZoneMap(
  val zones: Array[Zone], mapping: immutable.SortedMap[Road, immutable.SortedSet[Zone]],
  val links: immutable.SortedMap[Zone, immutable.SortedSet[Zone]]
) {
  def apply(r: Road): immutable.SortedSet[Zone] = mapping(r)
  // Since the sets are sorted by ID, this'll be deterministic
  def canonical(r: Road) = mapping(r).head

  def serialize(w: MapStateWriter) {
    w.int(zones.size)
    zones.foreach(z => z.serialize(w))
    w.int(mapping.size)
    for ((r, zlist) <- mapping) {
      w.int(w.roads(r.id).int)
      w.int(zlist.size)
      zlist.foreach(z => w.int(z.id.int))
    }
    w.int(links.size)
    for ((z, zlist) <- links) {
      w.int(z.id.int)
      w.int(zlist.size)
      zlist.foreach(z => w.int(z.id.int))
    }
  }
}

class Zone(val id: ZoneID, val roads: immutable.SortedSet[Road], val center: Coordinate)
  extends Ordered[Zone] with Renderable
{
  override def toString = s"Z$id"
  override def compare(other: Zone) = id.int.compare(other.id.int)

  def freeflow_time = 1.0 // TODO how to define the approx time to cross this zone?

  override def debug() {
    Util.log(s"Zone with ${roads.size}")
  }

  def serialize(w: MapStateWriter) {
    w.int(id.int)
    w.int(roads.size)
    roads.foreach(r => w.int(w.roads(r.id).int))
    center.serialize(w)
  }
}

object Zone {
  def unserialize(r: StateReader, roads: Array[Road]) = new Zone(
    new ZoneID(r.int), Util.sorted_set(Range(0, r.int).map(_ => roads(r.int))),
    Coordinate.unserialize(r)
  )
}

object ZoneMap {
  private val max_size = 350

  def create(roads: Array[Road]): ZoneMap = {
    val mapping = partition(roads)

    // Roads lead to different zones via ports, and some roads are in many zones. No self-cycles.
    def zone_succs(zone: Zone) = Util.sorted_set(
      ports(zone).flatMap(_.succs).flatMap(r => mapping(r)) ++ zone.roads.flatMap(r => mapping(r))
      - zone
    )
    // Member roads that have successors outside the set. Members shared with other zones aren't
    // included.
    def ports(zone: Zone)
      = zone.roads.filter(r => r.succs.exists(succ => !zone.roads.contains(succ)))

    val zones = mapping.values.flatten.toSet.toArray.sortBy(_.id.int)
    val links = Util.sorted_map(zones.map(zone => zone -> zone_succs(zone)))
    Util.log("%,d DRs partitioned into %,d zones with %,d connections".format(
      roads.size, zones.size, links.values.map(_.size).sum
    ))
    return new ZoneMap(zones, mapping, links)
  }

  def create_blank(roads: Array[Road]) =
    new ZoneMap(Array(), Util.sorted_map(Nil), Util.sorted_map(Nil))

  // TODO dont cross different road types?
  // TODO possibly nondeterministic, use trees?
  private def partition(roads: Array[Road]): immutable.SortedMap[Road, immutable.SortedSet[Zone]] =
  {
    Util.log("Partitioning the map into zones...")

    // Since "zones" are mutable during the building process, introduce a layer of introduction via
    // temporary zone ID ints.
    var next_id = 0
    val zone_members = new mutable.HashMap[ZoneID, mutable.Set[Road]]()
    val road_mapping = roads.map(r => r -> new mutable.HashSet[ZoneID]()).toMap

    val open = new mutable.TreeSet[Road]()  // TODO queue?
    open ++= roads
    while (open.nonEmpty) {
      print(s"\r  ${open.size} roads left to process")
      val base = open.head
      // TODO path now includes start step 'base'. relevant?
      val path = AStar.path(Pathfind(
        start = base, goal = base,
        calc_cost = (_: Road, next: Road, _: Double) => next.freeflow_time,
        calc_heuristic = (state: Road) => state.end_pt.dist_to(base.end_pt),
        allow_cycles = true
      )).path
      val new_zone = new mutable.HashSet[Road]()
      new_zone ++= path
      open --= new_zone

      // Merge zones by finding common overlap
      val overlapping_zones = Util.sorted_set(path.flatMap(r => road_mapping(r)))
      for (candidate <- overlapping_zones) {
        if ((new_zone ++ zone_members(candidate)).size < max_size) {
          new_zone ++= zone_members(candidate)
          // Since we absorbed this old zone in the new one, delete the existence of the old.
          zone_members(candidate).foreach(r => road_mapping(r) -= candidate)
          zone_members -= candidate
        }
      }
      val new_id = new ZoneID(next_id + 1)
      next_id += 1
      zone_members(new_id) = new_zone
      new_zone.foreach(r => road_mapping(r) += new_id)
    }
    println("")
    val zones = zone_members.keys.zipWithIndex.map(
      tupled((id, idx) => id -> new Zone(
        new ZoneID(idx), Util.sorted_set(zone_members(id)), compute_center(zone_members(id))
      ))
    ).toMap
    return Util.sorted_map(roads.map(
      r => r -> Util.sorted_set(road_mapping(r).map(id => zones(id)))
    ))
  }

  private def compute_center(roads: Iterable[Road]): Coordinate = {
    val pts = roads.map(r => r.rightmost.approx_midpt)
    val avg_x = pts.map(_.x).sum / roads.size
    val avg_y = pts.map(_.y).sum / roads.size
    return new Coordinate(avg_x, avg_y)
  }

  def unserialize(r: StateReader, roads: Array[Road]): ZoneMap = {
    val zones = Range(0, r.int).map(_ => Zone.unserialize(r, roads)).toArray
    val mapping = Range(0, r.int).map(_ => {
      roads(r.int) -> Util.sorted_set(Range(0, r.int).map(_ => zones(r.int)))
    })
    val links = Range(0, r.int).map(_ => {
      zones(r.int) -> Util.sorted_set(Range(0, r.int).map(_ => zones(r.int)))
    })
    return new ZoneMap(zones, Util.sorted_map(mapping), Util.sorted_map(links))
  }
}

// Lazily computes an actual path to get through each zone
/*class ZoneRouter(graph: Graph) extends Router(graph) {
  private val zone_map = graph.zones
  // TODO savestate
  // Caching saves some time, and it also avoids bouncing between local maxima
  private var zpath: List[Zone] = Nil

  override def router_type = RouterType.Zone

  override def path(from: Road, to: Road, time: Double, banned: Set[Road]): List[Road] = {
    if (zone_map(from).intersect(zone_map(to)).nonEmpty) {
      // In the same zone! Be normal now.
      return AStar.path(
        from, Set(to), (step: Road) => step.succs,
        // TODO heuristics? congestion?
        (_: Road, next: Road, _: (Double, Double)) => (next.freeflow_time, 0),
        (state: Road) => (state.end_pt.dist_to(to.end_pt), 0)
      )
    } else {
      val current_zone = zone_map.canonical(from)
      var path_tail = zpath.span(z => z != current_zone)._2
      if (path_tail.headOption.getOrElse(null) != current_zone) {
        zpath = current_zone :: zone_path(current_zone, zone_map.canonical(to))
        path_tail = zpath
      }
      // The "next" zone might not be straightforward since roads overlap
      val target_zone = path_tail.find(z => !z.roads.contains(from)).get
      // Route to anywhere in the target zone, but stay in the current zone
      return AStar.path(
        from, target_zone.roads, (step: Road) => step.succs,
        // TODO heuristics? congestion?
        (_: Road, next: Road, _: (Double, Double)) =>
          (Util.bool2binary(!current_zone.roads.contains(next)), next.freeflow_time),
        (state: Road) => (0, state.end_pt.dist_to(target_zone.center))
      )
    }
  }

  // Includes start as the first zone
  private def zone_path(start: Zone, goal: Zone) = AStar.path(
    start, Set(goal), (step: Zone) => zone_map.links(step),
    // TODO some notion of congestion within a zone
    (_: Zone, next: Zone, _: (Double, Double)) => (next.freeflow_time, 0),
    (state: Zone) => (state.center.dist_to(goal.center), 0)
  )
}*/
