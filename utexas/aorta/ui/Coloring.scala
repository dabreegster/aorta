// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import swing.{MenuItem, Action}
import java.awt.Color
import scala.collection.mutable

import utexas.aorta.map.{Vertex, Road}

import utexas.aorta.common.{cfg, AgentID}

// Callers can create multiple "layers" (just named color schemes) and then select colors from a
// layer
class RoadColorScheme(state: GuiState) {
  private val layers = new mutable.HashMap[String, mutable.HashMap[Road, Color]]()
  // A silly hack because it's obnoxious to filter through a Swing component
  private val menu_items = new mutable.HashMap[String, MenuItem]()

  private val no_layer = "Don't show any special layer"
  add_layer(no_layer)
  state.cur_layer = no_layer

  def add_layer(name: String) {
    layers(name) = new mutable.HashMap[Road, Color]()
    val item = new MenuItem(Action(name) {
      state.set_cur_layer(name)
      state.canvas.repaint()
    })
    GUI.layer_menu.contents += item
    menu_items(name) = item
  }
  def del_layer(name: String) {
    if (layers.contains(name)) {
      layers -= name
      // Can't just do -=
      GUI.layer_menu.peer.remove(menu_items.remove(name).get.peer)
      if (state.cur_layer == name) {
        state.cur_layer = no_layer
      }
    }
  }
  def set(layer: String, r: Road, c: Color) {
    layers(layer)(r) = c
  }
  def unset(layer: String, r: Road) {
    layers(layer) -= r
  }
  def change_cur_layer(name: String) {
    menu_items(state.cur_layer).action.title = state.cur_layer
    menu_items(name).action.title = s"[current] $name"
  }

  def get(layer: String, r: Road): Option[Color] = layers.get(layer).flatMap(_.get(r))
}

object DriverColorScheme {
  def color(d: DrawDriver, state: GuiState) = Stream(
    AccelerationScheme.color _, FocusVertexScheme.color _, CameraScheme.color _,
    StalledScheme.color _, PersonalScheme.color _
  ).flatMap(fxn => fxn(d, state)).head
}

// Reveal who's acting differently from their past life
// TODO disable sometimes. also, revive this?
/*object ReplayDiffScheme {
  private val deltas = new AgentMap[Double](0.0)

  def add_delta(id: AgentID, delta: Double) {
    deltas.put(id, deltas.get(id) + delta)
  }

  def color(d: DrawDriver, state: GuiState) =
    if (state.canvas.zoomed_in)
      FocusVertexScheme.color(d, state)
    else deltas.get(d.agent.id) match {
      case 0.0 => Color.GRAY
      case x if x < 0.0 => Color.RED
      case _ => Color.GREEN
    }
}*/

object AccelerationScheme {
  var enabled = false

  def color(d: DrawDriver, state: GuiState) =
    if (!enabled)
      None
    else if (d.agent.target_accel > 0)
      Some(Color.GREEN)
    else if (d.agent.target_accel == 0)
      Some(Color.BLUE)
    else
      Some(Color.RED)
}

// Focus on tickets at one intersection
object FocusVertexScheme {
  def color(d: DrawDriver, state: GuiState) = state.current_obj match {
    case Some(v: Vertex) => d.agent.all_tickets(v.intersection).toList match {
      case Nil => Some(Color.GRAY)
      case ls if ls.exists(_.is_approved) => Some(Color.GREEN)
      case ls if ls.exists(_.is_interruption) => Some(Color.YELLOW)
      case _ => Some(Color.RED)
    }
    case _ => None
  }
}

// Focus on the agent followed by the camera
object CameraScheme {
  def color(d: DrawDriver, state: GuiState) = state.camera_agent match {
    case Some(a) if d.agent == a => Some(Color.WHITE)
    case _ => None
  }
}

// Focus on cars that aren't moving
object StalledScheme {
  def color(d: DrawDriver, state: GuiState) =
    if (d.agent.how_long_idle >= 30.0)
      Some(Color.RED)
    else
      None
}

// Color each car individually
object PersonalScheme {
  def color(d: DrawDriver, state: GuiState) =
    if (state.canvas.zoomed_in)
      Some(d.personal_color)
    else
      Some(Color.BLUE)
}
