// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import swing.Dialog
import swing.event.Key
import scala.swing.FileChooser
import scala.util.Try
import java.io.File

import utexas.aorta.sim.Simulation
import utexas.aorta.common.{Util, RoadID, VertexID, EdgeID, cfg}

trait Controls {
  // TODO could use "self: OrganizerClass =>" pattern...
  private var state: GuiState = null
  private var canvas: MapCanvas = null
  private def sim(): Simulation = canvas.sim  // TODO whys compiler think this is recursive?

  def setup_controls(s: GuiState, c: MapCanvas) {
    state = s
    canvas = c
  }

  def handle_ev(ev: UI_Event): Unit = ev match {
    case EV_Action(action) => handle_ev_action(action)
    case EV_Key_Press(key) => handle_ev_keypress(key)
    case EV_Mouse_Moved(x, y) => {
      if (canvas.zoomed_in) {
        state.redo_mouseover(x, y)
      }
      canvas.repaint()
    }
    case EV_Param_Set("highlight", value) => {
      state.highlight_type = value
      canvas.repaint()
    }
    case EV_Select_Polygon_For_Serialization(polygon) => {
      val dir = s"maps/area_${sim.graph.name}"
      Util.mkdir(dir)
      for (name <- Dialog.showInput(message = "Name this area", initial = "")) {
        /*val edges = sim.graph.vertices.filter(
          v => polygon.contains(v.location.x, v.location.y)).flatMap(v => v.edges
        ).map(_.id).toArray*/
        val ways = sim.graph.vertices
          .filter(v => polygon.contains(v.location.x, v.location.y))
          .flatMap(v => v.roads).map(_.osm_id).toSet
        val w = Util.writer(s"${dir}/${name}")
        w.int(ways.size)
        ways.foreach(way => w.string(way))

        w.done()
        Util.log(s"Area saved to ${dir}/${name}")
      }
    }
    case _ =>
  }

  def handle_ev_action(ev: String): Unit = ev match {
    case "step" => canvas.repaint()
    case "toggle-running" => {
      // TODO move toggle method to state
      if (state.running) {
        state.running = false
        StatusBar.sim_speed.text = s"Paused / ${state.speed_cap}"
      } else {
        state.running = true
      }
    }
    case "clear-route" => {
      state.chosen_edge1 = None
      state.chosen_edge2 = None
      state.road_colors.del_layer("route")
      canvas.repaint()
    }
    // TODO refactor the 4 teleports?
    // TODO center on some part of the thing and zoom in?
    case "teleport-edge" => {
      for (id <- prompt_int("What edge ID do you seek?");
           e <- Try(sim.graph.get_e(new EdgeID(id))))
      {
        // just vaguely moving that way
        Util.log("Here's " + e)
        canvas.center_on(e.lines.head.start)
        state.chosen_edge1 = Some(e)  // just kind of use this to highlight it (a hack)
        canvas.repaint()
      }
      canvas.grab_focus()
    }
    case "teleport-road" => {
      for (id <- prompt_int("What road ID do you seek?");
           r <- Try(sim.graph.get_r(new RoadID(id))))
      {
        Util.log("Here's " + r)
        canvas.center_on(r.rightmost.approx_midpt)
        state.chosen_road = Some(r)
        canvas.repaint()
      }
      canvas.grab_focus()
    }
    case "teleport-agent" => {
      for (id <- prompt_int("What agent ID do you seek?"); a <- sim.get_agent(id)) {
        Util.log("Here's " + a)
        state.current_obj = Some(a)
        handle_ev_keypress(Key.F)
        canvas.repaint()
      }
      canvas.grab_focus()
    }
    case "teleport-vertex" => {
      for (id <- prompt_int("What vertex ID do you seek?");
           v <- Try(sim.graph.get_v(new VertexID(id.toInt))))
      {
        Util.log("Here's " + v)
        canvas.center_on(v.location)
        canvas.repaint()
      }
      canvas.grab_focus()
    }
  }

  // Unused letter keys: a b e h j k l m n o r u v
  def handle_ev_keypress(key: Any): Unit = key match {
    // TODO this'll be tab someday, i vow!
    case Key.Control => {
      // cycle through turns
      // TODO move it to be a method in state or somewhere
      state.current_edge match {
        case Some(e) => {
          state.current_turn += 1
          if (state.current_turn >= e.next_turns.size) {
            state.current_turn = 0
          }
          canvas.repaint()
        }
        case None =>
      }
    }
    case Key.P => handle_ev(EV_Action("toggle-running"))
    case Key.C if state.current_edge.isDefined => (state.chosen_edge1, state.chosen_edge2) match {
      case (Some(source), None) => {
        state.chosen_edge2 = state.current_edge
        // TODO later, let this inform any client
        canvas.show_pathfinding(source.road, state.current_edge.get.road)
      }
      case (None, None) | (Some(_), Some(_))=> {
        Util.log("Press 'c' again to choose a target road for pathfinding")
        state.road_colors.del_layer("route")
        state.chosen_edge1 = state.current_edge
        state.chosen_edge2 = None
        canvas.repaint()
      }
      case _ => // (None, Some) should be impossible
    }
    case Key.OpenBracket => {
      state.speed_cap = math.max(0, state.speed_cap - 1)
      canvas.update_status()
    }
    case Key.CloseBracket => {
      state.speed_cap += 1
      canvas.update_status()
    }
    case Key.D => state.current_obj match {
      case Some(thing) => thing.debug
      case None =>
    }
    case Key.F => {
      // Unregister old listener
      state.camera_agent match {
        case Some(a) => a.set_debug(false)
        case None =>
      }

      state.camera_agent = state.current_agent
      state.camera_agent match {
        case Some(a) => {
          a.set_debug(true)
          state.road_colors.add_layer("route")
          a.route.current_path.foreach(r => state.road_colors.set("route", r, cfg.route_member_color))
          state.set_cur_layer("route")
          canvas.repaint()
        }
        case None => {
          state.road_colors.del_layer("route")
        }
      }
    }
    // TODO convert some/many of these to menus
    case Key.X => {
      // Launch this in a separate thread so the sim can be continued normally
      new Thread {
        override def run() {
          new GUIREPL(sim).run()
        }
      }.start()
    }
    case Key.G => state.show_green = !state.show_green
    case Key.T => state.show_tooltips = !state.show_tooltips
    case Key.Z => state.show_zone_colors = !state.show_zone_colors
    case Key.W => state.show_zone_centers = !state.show_zone_centers
    case Key.S => {
      if (!state.running) {
        canvas.step_sim()
      }
    }
    case Key.I => AccelerationScheme.enabled = !AccelerationScheme.enabled
    case Key.Y => canvas.show_road_usage()
    case Key.Q => canvas.show_tolls()
    case _ =>
  }

  // Utility methods, move em somewhere general? They were in ScrollingCanvas
  private def prompt_int(msg: String) =
    Dialog.showInput(message = msg, initial = "").map(num => Try(num.toInt).toOption).flatten
  private def prompt_double(msg: String) =
    Dialog.showInput(message = msg, initial = "").map(num => Try(num.toDouble).toOption).flatten
  def prompt_fn(title: String = ""): Option[String] = {
    val chooser = new FileChooser(new File("."))
    chooser.title = title
    return chooser.showOpenDialog(null) match {
      case FileChooser.Result.Approve => Some(chooser.selectedFile.getPath)
      case _ => None
    }
  }
}
