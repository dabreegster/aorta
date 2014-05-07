// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable
import utexas.aorta.map.Coordinate
import utexas.aorta.common.Util

class BuildingScraper() {
  // For now, just store the "center" point associated with the building
  case class Bldg(road: Option[String], point: Coordinate, residential: Boolean)
  private val bldgs = new mutable.ListBuffer[Bldg]()

  // TODO missing any?
  private val bldg_tags = Set("addr:housenumber", "shop")
  private val nonresidence_tags = Set("amenity", "shop")
  private def is_bldg(tags: Map[String, String]) = tags.keys.exists(bldg_tags.contains(_))
  private def is_residential(tags: Map[String, String]) =
    !tags.keys.exists(nonresidence_tags.contains(_))

  def scrape(osm: OsmReader) {
    osm.listen(classOf[EV_OSM], _ match {
      // Grab an arbitrary point from the building
      case EV_OSM(elem) if is_bldg(elem.tags) =>
        bldgs += Bldg(elem.tags.get("addr:street"), find_center(elem.points), is_residential(elem.tags))
      case _ =>
    })
  }

  // Not really the center? Something.
  private def find_center(points: List[Coordinate]): Coordinate = {
    val min_x = points.map(_.x).min
    val min_y = points.map(_.y).min
    val max_x = points.map(_.x).max
    val max_y = points.map(_.y).max
    return Coordinate((min_x + max_x) / 2, (min_y + max_y) / 2)
  }

  def normalize_coords(fix: (Coordinate) => Coordinate) {
    val fixed = bldgs.map(bldg => bldg.copy(point = fix(bldg.point)))
    bldgs.clear()
    bldgs ++= fixed
  }

  def group(graph: PreGraph3) {
    Util.log(s"Matching ${bldgs.size} buildings to roads...")
    // First group roads by their name for fast pruning
    val roads_by_name = graph.roads.groupBy(_.name)
    var skipped = 0
    for (bldg <- bldgs) {
      // for now, ignore roads without a name, or with a name we don't know
      if (bldg.road.isDefined && roads_by_name.contains(bldg.road.get)) {
        val candidates = roads_by_name(bldg.road.get)
        val r = candidates.minBy(r => r.lanes.last.approx_midpt.dist_to(bldg.point))
        if (bldg.residential) {
          r.houses += bldg.point
        } else {
          r.shops += bldg.point
        }
      } else {
        skipped += 1
      }
    }
    Util.log(s"Skipped ${skipped} buildings that couldn't be matched to roads by name")
  }
}
