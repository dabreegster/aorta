// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._
import scala.collection.mutable
import java.util.NoSuchElementException

import utexas.aorta.map.Coordinate
import utexas.aorta.common.Publisher

trait OsmElement {
  val tags: Map[String, String]
  def points: List[Coordinate]
}
final case class EV_OSM(elem: OsmElement)

case class OsmNode(id: String, lon: Double, lat: Double, tags: Map[String, String])
  extends OsmElement
{
  override def points = List(coordinate)
  def coordinate = Coordinate(lon, lat)
}

case class OsmWay(id: String, refs: List[OsmNode], tags: Map[String, String]) extends OsmElement {
  override def points = refs.map(_.coordinate)
  def name = tags.getOrElse("name", tags.getOrElse("ref", s"NO-NAME (OSM $id)"))
  def road_type = tags.getOrElse("highway", "null")
  def oneway = tags.getOrElse("oneway", "") == "yes" || Pass1.forced_oneways(road_type)
  // TODO fancy "x; y" format... handle eventually
  def lanes = try {
    Some(tags.getOrElse("lanes", "").toInt)
  } catch {
    case _: NumberFormatException => None
  }
}

case class OsmRelation(
  id: String, member_ways: List[OsmWay], member_nodes: List[OsmNode], tags: Map[String, String]
) extends OsmElement {
  override def points = member_ways.flatMap(_.points) ++ member_nodes.map(_.coordinate)
}

class OsmReader(fn: String) extends Publisher {
  val id_to_node = new mutable.HashMap[String, OsmNode]()
  val id_to_way = new mutable.HashMap[String, OsmWay]()

  def parse() {
    val xml = new XMLEventReader(Source.fromFile(fn, "UTF-8")).buffered
    var toplevel_count = 0
    while (xml.hasNext) {
      xml.next match {
        case EvElemStart(_, "node", attribs, _) => read_node(attribs, xml).foreach(node => {
          publish(EV_OSM(node))
        })
        case EvElemStart(_, "way", attribs, _) => read_way(attribs, xml).foreach(way => {
          publish(EV_OSM(way))
        })
        case EvElemStart(_, "relation", attribs, _) => read_relation(attribs, xml).foreach(rel => {
          publish(EV_OSM(rel))
        })
        case _ =>
      }
      toplevel_count += 1
      if (toplevel_count % 1000 == 0) {
        // it's expensive to spam System.out, believe it or not :P
        print(f"\rProcessed $toplevel_count%,d top-level XML objects")
      }
    }
    println("")
  }

  private type XMLIter = BufferedIterator[XMLEvent]

  private def get(attribs: MetaData, key: String): String = attribs.get(key).head.text
  private def get(attribs: MetaData, key: String, default: String): String =
    attribs.get(key) match {
      case Some(ls) => ls.head.text
      case None => default
    }

  private def read_tags(xml: XMLIter): Map[String, String] = {
    val tags = new mutable.HashMap[String, String]()
    while (xml.hasNext) {
      xml.head match {
        case EvElemStart(_, "tag", attribs, _) => {
          tags(get(attribs, "k")) = get(attribs, "v")
          xml.next()
        }
        case EvElemEnd(_, "tag") => xml.next()
        case EvText(_) => xml.next()
        case _ => return tags.toMap
      }
    }
    throw new IllegalArgumentException("XML ended with tags")
  }

  private def read_node(attribs: MetaData, xml: XMLIter): Option[OsmNode] = {
    if (get(attribs, "visible", "true") != "true" || get(attribs, "action", "modify") != "modify") {
      return None
    }
    val tags = read_tags(xml)
    xml.next match {
      case EvElemEnd(_, "node") =>
    }
    val node = OsmNode(
      get(attribs, "id"), get(attribs, "lon").toDouble, get(attribs, "lat").toDouble, tags
    )
    id_to_node(node.id) = node
    return Some(node)
  }

  private def read_way(attribs: MetaData, xml: XMLIter): Option[OsmWay] = {
    if (get(attribs, "visible", "true") != "true" || get(attribs, "action", "modify") != "modify") {
      return None
    }
    // Is this always the order?
    val refs = try {
      read_refs(xml)
    } catch {
      case _: NoSuchElementException => return None
    }
    val tags = read_tags(xml)
    xml.next match {
      case EvElemEnd(_, "way") =>
    }

    val way = OsmWay(get(attribs, "id"), refs, tags)
    id_to_way(way.id) = way
    return Some(way)
  }

  private def read_refs(xml: XMLIter): List[OsmNode] = {
    val ls = new mutable.ListBuffer[OsmNode]()
    while (xml.hasNext) {
      xml.head match {
        case EvElemStart(_, "nd", attribs, _) => {
          ls += id_to_node(get(attribs, "ref"))
          xml.next()
        }
        case EvElemEnd(_, "nd") => xml.next()
        case EvText(_) => xml.next()
        case _ => return ls.toList
      }
    }
    throw new IllegalArgumentException("XML ended with node refs")
  }

  private def read_relation(attribs: MetaData, xml: XMLIter): Option[OsmRelation] = {
    if (get(attribs, "visible", "true") != "true" || get(attribs, "action", "modify") != "modify") {
      return None
    }
    // Order?
    val (ways, nodes) = try {
      read_members(xml)
    } catch {
      case _: NoSuchElementException => return None
    }
    val tags = read_tags(xml)
    xml.next match {
      case EvElemEnd(_, "relation") =>
    }

    return Some(OsmRelation(get(attribs, "id"), ways, nodes, tags))
  }

  private def read_members(xml: XMLIter): (List[OsmWay], List[OsmNode]) = {
    val ways = new mutable.ListBuffer[OsmWay]()
    val nodes = new mutable.ListBuffer[OsmNode]()
    while (xml.hasNext) {
      xml.head match {
        case EvElemStart(_, "member", attribs, _) => {
          (get(attribs, "type"), get(attribs, "ref")) match {
            case ("way", ref) => ways += id_to_way(ref)
            case ("node", ref) => nodes += id_to_node(ref)
            case ("relation", _) => // super-relations! whoa!
          }
          xml.next()
        }
        case EvElemEnd(_, "member") => xml.next()
        case EvText(_) => xml.next()
        case _ => return (ways.toList, nodes.toList)
      }
    }
    throw new IllegalArgumentException("XML ended with member relations")
  }
}
