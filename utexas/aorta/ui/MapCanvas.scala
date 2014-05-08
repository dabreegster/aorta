// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import scala.collection.mutable
import java.awt.{Graphics2D, Shape, BasicStroke, Color}
import java.awt.geom.{Rectangle2D, Ellipse2D, Line2D}
import swing.event.Key

import utexas.aorta.map.{Road, Vertex, Edge, Position, Turn, Coordinate}
import utexas.aorta.sim.{Simulation, EV_Signal_Change, EV_Transition, EV_Reroute, EV_Breakpoint,
                         EV_Heartbeat, AgentMap}
import utexas.aorta.sim.make.IntersectionType
import utexas.aorta.sim.drivers.Agent

import utexas.aorta.common.{Util, cfg}

// Cleanly separates GUI state from users of it
class GuiState(val canvas: MapCanvas) {
  // Per-render state
  var g2d: Graphics2D = null
  val tooltips = new mutable.ListBuffer[Tooltip]()

  // Permanent state
  var show_tooltips = true
  var current_obj: Option[Renderable] = None
  var camera_agent: Option[Agent] = None
  var chosen_road: Option[Road] = None
  var highlight_type: Option[String] = None
  var polygon_roads1: Set[Road] = Set()
  var polygon_roads2: Set[Road] = Set()
  var chosen_edge1: Option[Edge] = None
  var chosen_edge2: Option[Edge] = None
  var running = false
  var speed_cap: Int = 1  // A rate of realtime. 1x is realtime.
  var current_turn = -1  // for cycling through turns from an edge
  var show_green = false
  var cur_layer = ""
  // Has to be after cur_layer so it's defined :\
  val road_colors = new RoadColorScheme(this)

  // Actions
  def reset(g: Graphics2D) {
    g2d = g
    tooltips.clear()
  }

  def set_cur_layer(layer: String) {
    road_colors.change_cur_layer(layer)
    cur_layer = layer
  }

  def redo_mouseover(x: Double, y: Double) {
    current_obj = None
    current_turn = -1
    val sim = canvas.sim
    val cursor = new Rectangle2D.Double(x - eps, y - eps, eps * 2, eps * 2)

    // TODO ideally, center agent bubble where the vehicle center is drawn.
    // Order of search: agents, vertices, edges, roads
    current_obj =
      canvas.driver_renderers.values.find(_.hits(cursor)).map(_.agent)
      .orElse(sim.graph.vertices.find(v => bubble(v.location).intersects(cursor)))
      .orElse(canvas.road_renderers.flatMap(_.edges).find(_.hits(cursor))
              .map(e => Position(e.edge, e.edge.approx_dist(Coordinate(x, y), 1.0))))
      .orElse(canvas.road_renderers.find(_.hits(cursor)).map(_.r))
  }

  // Queries
  def current_edge: Option[Edge] = current_obj match {
    case Some(pos: Position) => Some(pos.on.asEdge)
    case _ => None
  }
  def current_agent: Option[Agent] = current_obj match {
    case Some(a: Agent) => Some(a)
    case _ => None
  }

  // the radius of a small epsilon circle for the cursor
  def eps = 5.0 / canvas.zoom
  def bubble(pt: Coordinate) = new Ellipse2D.Double(pt.x - eps, pt.y - eps, eps * 2, eps * 2)
}

class MapCanvas(val sim: Simulation, headless: Boolean = false)
  extends ScrollingCanvas with Controls with Visualization
{
  //////////////////////////////////////////////////////////////////////////////
  // State
  private val state = new GuiState(this)

  val driver_renderers = new AgentMap[DrawDriver](null) {
    override def when_created(a: Agent) {
      put(a.id, new DrawDriver(a, state))
    }
  }
  val road_renderers = sim.graph.roads.map(r => new DrawRoad(r, state))
  private val road_lookup = road_renderers.map(r => r.r -> r).toMap
  private val artifact_renderers = sim.graph.artifacts.map(a => new DrawRoadArtifact(a, state))
  private val drawing_stroke = new BasicStroke(
    5.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, Array(1.0f), 0.0f
  )

  private var last_tick = 0.0
  private var last_render: Long = 0
  private var tick_last_render = 0.0

  private val green_turns = new mutable.HashMap[Turn, Shape]()
  private val policy_colors = Map(
    IntersectionType.StopSign -> cfg.stopsign_color,
    IntersectionType.Signal -> cfg.signal_color,
    IntersectionType.Reservation -> cfg.reservation_color,
    IntersectionType.Yield -> cfg.yield_color,
    IntersectionType.AIM -> cfg.aim_color,
    IntersectionType.Batch -> cfg.batch_color
  )

  //////////////////////////////////////////////////////////////////////////////
  // Setup
  setup_controls(state, this)
  setup_viz(state, this)
  driver_renderers.create_from_existing(sim)
  // begin in the center
  x_off = canvas_width / 2
  y_off = canvas_height / 2

  sim.listen(classOf[EV_Heartbeat], _ match { case e: EV_Heartbeat => {
    update_status()
    StatusBar.agents.text = e.describe
    StatusBar.time.text = Util.time_num(e.tick)
    last_tick = e.tick
  }})
  sim.listen(classOf[EV_Breakpoint], _ match { case EV_Breakpoint(a) => {
    println(s"Pausing to target $a")
    state.current_obj = Some(a)
    handle_ev_keypress(Key.F)
    pause()
    // Launch this in a separate thread so the sim can be continued normally
    new Thread {
      override def run() {
        new GUIREPL(sim).run()
      }
    }.start()
  }})
  sim.listen(classOf[EV_Reroute], _ match {
    case EV_Reroute(agent, path, _, _, _, _) if agent == state.camera_agent.getOrElse(null) => {
      state.road_colors.add_layer("route")
      path.foreach(r => state.road_colors.set("route", r, cfg.route_member_color))
    }
    case _ =>
  })
  sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(agent, from, to) if agent == state.camera_agent.getOrElse(null) => from match {
      case e: Edge => {
        state.road_colors.unset("route", e.road)
      }
      case _ =>
    }
    case _ =>
  })

  // Headless mode might be controlling us...
  if (!headless) {
    // fire steps every now and then
    new Thread {
      override def run() {
        while (true) {
          if (state.running && state.speed_cap > 0) {
            val start_time = System.currentTimeMillis
            step_sim()

            // Rate-limit, if need be.
            // In order to make speed_cap ticks per second, each tick needs to
            // last 1000 / speed_cap milliseconds.
            val goal = 
              if (state.speed_cap > 0)
                (1000 / state.speed_cap).toInt
              else
                0
            val dt_ms = System.currentTimeMillis - start_time
            if (dt_ms < goal) {
              // Ahead of schedule. Sleep.
              Thread.sleep(goal - dt_ms)
            }
          } else {
            // Just avoid thrashing the CPU.
            Thread.sleep(100)
          }
        }
      }
    }.start()
  }

  sim.listen(classOf[EV_Signal_Change], _ match { case EV_Signal_Change(greens) => {
    green_turns.clear()
    for (t <- greens) {
      green_turns(t) = GeomFactory.line2awt(GeomFactory.turn_body(t))
    }
  }})
  // At this point, signal policies have already fired up and sent the first
  // round of greens. We missed it, so compute manually the first time.
  // TODO better solution
  for (v <- sim.graph.vertices) {
    for (t <- v.intersection.policy.current_greens) {
      green_turns(t) = GeomFactory.line2awt(GeomFactory.turn_body(t))
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def step_sim() {
    sim.step()
    state.camera_agent match {
      case Some(a) => {
        if (sim.has_agent(a)) {
          center_on(a.at.location)
        } else {
          Util.log(a + " is done; the camera won't stalk them anymore")
          state.camera_agent = None
          state.road_colors.del_layer("route")
        }
      }
      case None =>
    }

    // Only render every 0.2 seconds
    val now = System.currentTimeMillis
    if (now - last_render > cfg.render_ms && sim.tick != tick_last_render ) {
      handle_ev(EV_Action("step"))
      last_render = now
      tick_last_render = sim.tick
    }
  }

  def update_status() {
    StatusBar.sim_speed.text = "%dx / %dx".format((sim.tick - last_tick).toInt, state.speed_cap)
  }

  override def render_canvas(g2d: Graphics2D, window: Rectangle2D.Double): List[Tooltip] = {
    state.reset(g2d)

    for (a <- artifact_renderers if a.hits(window)) {
      a.render_road()
    }

    val roads_seen = road_renderers.filter(r => {
      val hit = r.hits(window)
      if (hit) {
        r.render_road()
      }
      hit
    })

    // don't show tiny details when it doesn't matter (and when it's expensive
    // to render them all)
    if (zoomed_in) {
      for (r <- roads_seen) {
        r.render_edges()
        r.render_buildings()
      }

      state.current_obj match {
        case Some(pos: Position) => {
          val e = pos.on.asInstanceOf[Edge]
          DrawIntersection.draw_turns(state, e)
          highlight_buildings(g2d, e.road)
        }
        case Some(v: Vertex) => {
          for (t <- v.intersection.policy.current_greens) {
            DrawIntersection.draw_turn(state, t, cfg.turn_color)
          }
        }
        case _ =>
      }

      // Show traffic signal stuff
      if (state.show_green) {
        g2d.setStroke(GeomFactory.center_stroke)
        g2d.setColor(Color.GREEN)
        green_turns.foreach(t => if (t._2.intersects(window)) {
          g2d.fill(t._2)
          // TODO draw the tip too?
        })
      }

      // Illustrate the intersection policies
      for (v <- sim.graph.vertices) {
        val bub = state.bubble(v.location)
        if (bub.intersects(window)) {
          g2d.setColor(policy_colors(v.intersection.policy.policy_type))
          g2d.draw(bub)
        }
      }
    }

    for (driver <- driver_renderers.values) {
      if (driver.hits(window)) {
        driver.render()
      }
    }

    // Finally, if the user is free-handing a region, show their work.
    g2d.setColor(cfg.polygon_color)
    g2d.setStroke(drawing_stroke)
    g2d.draw(drawing_polygon)

    // What tooltips do we want?
    state.current_obj match {
      case Some(thing) => {
        state.tooltips += Tooltip(
          screen_to_map_x(mouse_at_x), screen_to_map_y(mouse_at_y),
          thing.tooltip, false
        )
        thing match {
          // TODO make all the moused over things be renderables with this method
          case a: Agent => driver_renderers.get(a).moused_over()
          case _ =>
        }
      }
      case None =>
    }
    return state.tooltips.toList
  }

  def highlight_buildings(g2d: Graphics2D, r: Road) {
    g2d.setColor(Color.RED)
    for (bldg <- r.shops ++ r.houses) {
      g2d.draw(state.bubble(bldg))
    }
  }

  def pause() {
    state.running = true
    handle_ev_action("toggle-running")
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def zoomed_in = zoom > cfg.zoom_threshold

  override def canvas_width = sim.graph.width.toInt
  override def canvas_height = sim.graph.height.toInt
}
