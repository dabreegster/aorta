// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import swing._  // TODO figure out exactly what
import java.awt.{Color, Component}
import swing.Dialog
import javax.swing.WindowConstants
import java.io.File

import utexas.aorta.sim.{Simulation, EV_Heartbeat}
import utexas.aorta.analysis.SimREPL
import utexas.aorta.common.{Util, cfg}

object StatusBar {
  val zoom       = new Label("1.0") // TODO from cfg
  val agents     = new Label("0 moved / 0 live / 0 ready")
  val time       = new Label("0.0")
  val sim_speed = new Label("Paused / 1x")

  val panel = new GridBagPanel {
    maximumSize = new Dimension(Int.MaxValue, 10)
    border = Swing.MatteBorder(5, 5, 5, 5, Color.BLACK)

    // TODO generate these?

    // all of this to prevent the rightmost 'At' column from spazzing out when
    // the text changes length
    // row 1: labels
    val c = new Constraints
    c.gridx = 0
    c.gridy = 0
    c.ipadx = 50
    layout(new Label("Zoom")) = c
    c.gridx = 1
    layout(new Label("Agents Active/Ready/Routing")) = c
    c.gridx = 2
    layout(new Label("Time")) = c
    c.gridx = 3
    layout(new Label("Sim Speed (Actual/Cap)")) = c

    // row 2: columns
    c.weightx = 0.0
    c.ipadx = 50
    c.gridx = 0
    c.gridy = 1
    layout(StatusBar.zoom) = c
    c.gridx = 1
    layout(StatusBar.agents) = c
    c.gridx = 2
    layout(StatusBar.time) = c
    c.gridx = 3
    layout(StatusBar.sim_speed) = c
  }
}

// TODO SwingApplication has a startup, quit, shutdown...
object GUI extends SimpleSwingApplication {
  // TODO pipe in differently
  val side_by_side = false

  val road_types = List(
    "null", "residential", "unclassified", "secondary",
    "motorway_link", "motorway", "trunk_link", "secondary_link", "primary_link",
    "tertiary", "primary", "service"
  )
  // null just because it's parametric from argv
  var primary_canvas_2d: MapCanvas = null
  // for side-by-side mode
  var secondary_canvas_2d: MapCanvas = null
  val layer_menu = new Menu("Road Color Layer")

  // TODO use this finally?
  val helper = new BoxPanel(Orientation.Vertical) {
    preferredSize = new Dimension(Int.MaxValue, Int.MaxValue)

    border = Swing.MatteBorder(5, 5, 5, 5, Color.BLACK)
    yLayoutAlignment = java.awt.Component.TOP_ALIGNMENT
    // TODO These're fixed now, but the idea is to tie them to configuration and
    // add/remove some context-sensitively. And also organize better.

    // Simulation controls
    contents += new Label("p   pause/resume")
    contents += new Label("[   slow down time")
    contents += new Label("]   speed up time")
    contents += new Label("-   slown down time faster")
    contents += new Label("=   speed up time faster")

    // Actions
    contents += new Label("m   make new agent on current edge")
    contents += new Label("c   choose edge for pathfinding")
    contents += new Label("d   object-sensitive debug")
    contents += new Label("f   follow agent")
    contents += new Label("x   delete agent (may crash!)")

    // Polygons
    contents += new Label("Shift+Left click   draw a polygon")
    contents += new Label("Shift+s   begin/end agents in polygon")
    contents += new Label("Shift+p   change intersection policies")

    // View
    contents += new Label("r   reset view")
    contents += new Label("g   toggle greenflood colors")
    contents += new Label("CTRL   cycle through turns")
    contents += new Label("arrow keys pan")

    // TODO expand to fill the whole column, or otherwise work on aesthetics
    // TODO and option to hide the panel
  }

  private var headless = false
  var closed = false

  override def main(args: Array[String]) {
    val sim = Util.process_args(args)
    primary_canvas_2d = new MapCanvas(sim)
    // TODO doesnt start drawn correctly!
    primary_canvas_2d.repaint
    super.main(args)
  }

  def launch_from_headless(canvas: MapCanvas) {
    headless = true
    primary_canvas_2d = canvas
    super.main(Array())
  }

  def top = new MainFrame {
    title = "AORTA"
    preferredSize = new Dimension(800, 600)
    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)

    override def closeOperation() {
      if (headless) {
        println("Closing GUI...")
        close
        closed = true
      } else {
        sys.exit
      }
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new Separator
        contents += new MenuItem(Action("Quit") {
          sys.exit
        })
      }

      contents += new Menu("View") {
        contents += new Menu("Highlight type of road") {
          contents ++= road_types.map(t => new MenuItem(Action(t) {
            primary_canvas_2d.handle_ev(EV_Param_Set("highlight", Some(t)))
          }))
        }
        contents += new MenuItem(Action("Clear all highlighting") {
          primary_canvas_2d.handle_ev(EV_Param_Set("highlight", None))
        })
      }

      contents += new Menu("Query") {
        contents += new MenuItem(Action("Teleport to Edge") {
          primary_canvas_2d.handle_ev(EV_Action("teleport-edge"))
        })
        contents += new MenuItem(Action("Teleport to Road") {
          primary_canvas_2d.handle_ev(EV_Action("teleport-road"))
        })
        contents += new MenuItem(Action("Teleport to Agent") {
          primary_canvas_2d.handle_ev(EV_Action("teleport-agent"))
        })
        contents += new MenuItem(Action("Teleport to Vertex") {
          primary_canvas_2d.handle_ev(EV_Action("teleport-vertex"))
        })

        contents += new MenuItem(Action("Clear Route") {
          primary_canvas_2d.handle_ev(EV_Action("clear-route"))
        })
      }
      contents += layer_menu
    }

    val main_split = new BorderPanel {
      background = Color.LIGHT_GRAY
      border = Swing.MatteBorder(2, 2, 2, 2, Color.RED)

      add(StatusBar.panel, BorderPanel.Position.North)
      add(primary_canvas_2d, BorderPanel.Position.Center)
    }

    if (side_by_side) {
      // TODO resizing entire window doesn't work great yet
      // TODO secondary_canvas_2d, not the helper. helper just to test.
      contents = new SplitPane(Orientation.Vertical, main_split, helper) {
        dividerLocation = 400
      }
    } else {
      contents = main_split
    }
  }
}

// TODO make pause work with whoever's calling us
class GUIDebugger(sim: Simulation) {
  // When this file exists, launch a GUI for sudden interactive watching.
  private val gui_signal = new File(".headless_gui")
  private var gui: Option[MapCanvas] = None

  sim.listen(classOf[EV_Heartbeat], _ match { case e: EV_Heartbeat => {
    if (gui_signal.exists) {
      gui_signal.delete()
      gui match {
        case Some(ui) => {
          if (GUI.closed) {
            println("Resuming the GUI...")
            GUI.top.open()
            GUI.closed = false
          }
        }
        case None => {
          println("Launching the GUI...")
          gui = Some(new MapCanvas(sim, headless = true))
          GUI.launch_from_headless(gui.get)
        }
      }
    }
    gui match {
      case Some(ui) if !GUI.closed => ui.handle_ev(EV_Action("step"))
      case _ =>
    }
  }})
}

class GUIREPL(sim: Simulation) extends SimREPL(sim) {
  override def welcome() {
    super.welcome()
    e.interpret("val gui = utexas.aorta.ui.GUI.canvas_2d")
    println("The GUI is bound to 'gui'.")
  }
}
