// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common

import java.awt.Color
import scala.io.Source

object cfg {
  private val cfg_fn = "aorta.cfg"
  private val color = Map(
    "RED" -> Color.RED,
    "GREEN" -> Color.GREEN,
    "BLUE" -> Color.BLUE,
    "YELLOW" -> Color.YELLOW,
    "PURPLE" -> new Color(204, 0, 255),
    "ORANGE" -> Color.ORANGE
  )
  private val params = load_config()

  def load_config(): Map[String, String] =
    Source.fromFile(cfg_fn).getLines().flatMap(line => kv(line)).toMap

  private def kv(line: String): Option[(String, String)] = {
    val cleaned = line.split("#").head.trim.replaceAll("""\s+""", " ")
    if (cleaned.isEmpty) {
      return None
    } else {
      val Array(key, value) = cleaned.split("=")
      return Some((key.trim, value.trim))
    }
  }

  private def bool(x: String) =
    if (x == "true")
      true
    else if (x == "false")
      false
    else
      throw new Exception(s"Bad boolean in config: $x")

  val antialias = bool(params("antialias"))
  val army_size = params("army_size").toInt
  val autosave_every = params("autosave_every").toDouble
  val replay_freq = params("replay_freq").toDouble
  val chosen_road_color = color(params("chosen_road_color"))
  val yield_color = color(params("yield_color"))
  val aim_color = color(params("aim_color"))
  val batch_color = color(params("batch_color"))
  val dash_center = bool(params("dash_center"))
  val draw_cursor = bool(params("draw_cursor"))
  val draw_lane_arrow = bool(params("draw_lane_arrow"))
  val dst_polygon_color = color(params("dst_polygon_color"))
  val dt_s = params("dt_s").toDouble
  val end_threshold = params("end_threshold").toDouble
  val epsilon = params("epsilon").toDouble
  val follow_dist = params("follow_dist").toDouble
  val lane_color = color(params("lane_color"))
  val lane_width = params("lane_width").toDouble
  val max_accel = params("max_accel").toDouble
  val max_lanes = params("max_lanes").toInt
  val min_road_len = params("min_road_len").toDouble
  val ordering = params("ordering")
  val policy = params("policy")
  val polygon_color = color(params("polygon_color"))
  val reservation_color = color(params("reservation_color"))
  val route_member_color = color(params("route_member_color"))
  val signal_color = color(params("signal_color"))
  val signal_duration = params("signal_duration").toInt
  val src_polygon_color = color(params("src_polygon_color"))
  val stopsign_color = color(params("stopsign_color"))
  val turn_color = color(params("turn_color"))
  val wallet = params("wallet")
  val zoom_threshold = params("zoom_threshold").toDouble
  val render_ms = params("render_ms").toInt

  val lanechange_dist = lane_width * params("lanechange_dist_rate").toDouble
  // Make sure time-based params are a multiple of dt_s.
  Util.assert_eq((autosave_every / dt_s).isValidInt, true)
  Util.assert_eq((replay_freq / dt_s).isValidInt, true)
}
