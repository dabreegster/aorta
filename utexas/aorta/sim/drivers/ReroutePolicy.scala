// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import utexas.aorta.map.{Edge, Turn, TollboothRouter, Road}
import utexas.aorta.sim.{EV_Transition, EV_Reroute}
import utexas.aorta.sim.intersections.{Ticket, Intersection}
import utexas.aorta.common.algorithms.Pathfind

// Responsible for requesting reroutes
abstract class ReroutePolicy(a: Agent) {
  protected var should_reroute = false
  def react() {
    if (should_reroute) {
      should_reroute = false

      // Find the first edge for which the driver has no tickets, so we don't have to cancel
      // anything
      var at = a.at.on
      while (true) {
        at match {
          case e: Edge => {
            if (e.road == a.route.goal) {
              return
            }
            a.get_ticket(e) match {
              case Some(ticket) => at = ticket.turn
              case None => {
                // Reroute from there
                a.route.optional_reroute(e)
                return
              }
            }
          }
          case t: Turn => at = t.to
        }
      }
    }
  }

  // only called for optional rerouting
  def approve_reroute(old_path: List[Road], new_path: List[Road]) = true

  // If we're part of gridlock and we have a choice, bail out. If not, totally fine to keep the
  // ticket.
  def pick_alt_turn(ticket: Ticket) =
    // Getting impatient? This is a late reaction to gridlock.
    if (ticket.turn.from.next_turns.size > 1 && Intersection.detect_gridlock(ticket.turn)) {
      val e = ticket.turn.from
      a.route.optional_reroute(e)
      a.route.pick_turn(e)
    } else {
      ticket.turn
    }

  def pick_next_turn(e: Edge): Turn = {
    val next_road = a.route.current_path.span(r => r != e.road)._2.tail.head
    if (next_road.congested) {
      a.route.optional_reroute(e)
    }
    return a.route.pick_turn(e)
  }
}

class NeverReroutePolicy(a: Agent) extends ReroutePolicy(a)

// Avoid perpetual oscillation by hysteresis
class RegularlyReroutePolicy(a: Agent) extends ReroutePolicy(a) {
  private var roads_crossed = 1
  private val reroute_frequency = 15
  private val hysteresis_threshold = 0.7

  a.sim.listen(classOf[EV_Transition], a, _ match {
    case EV_Transition(_, from, to: Turn) => {
      roads_crossed += 1
      if (roads_crossed % reroute_frequency == 0) {
        should_reroute = true
      }
    }
    case _ =>
  })

  override def approve_reroute(old_path: List[Road], new_path: List[Road]): Boolean = {
    // Score both paths
    // TODO this is tied to TollboothRouter, make it use the rerouter!
    val router = new TollboothRouter(a.sim.graph)
    router.setup(a)
    val cost_fxn = router.transform(Pathfind()).calc_cost
    val old_cost = old_path.zip(old_path.tail).map(pair => cost_fxn(pair._1, pair._2, 0)).sum
    val new_cost = new_path.zip(old_path.tail).map(pair => cost_fxn(pair._1, pair._2, 0)).sum

    // Lower is better, less cost per old cost
    val ratio = new_cost / old_cost
    return ratio < hysteresis_threshold
  }
}

// Reroute when a road in our path is raised drastically
// TODO also try subscribing to changes in individual roads
class PriceChangeReroutePolicy(a: Agent) extends ReroutePolicy(a) {
  private val rescan_time = 30
  private val cost_ratio_threshold = 1.5

  private var total_orig_cost = 0.0
  private var start_countdown = false
  private var rescan_countdown = rescan_time

  a.sim.listen(classOf[EV_Reroute], a, _ match {
    case ev: EV_Reroute => start_countdown = true
  })

  override def react() {
    // The driver rerouted, so scan the new cost and set a timer to check on things
    if (start_countdown) {
      total_orig_cost = calc_route_cost
      rescan_countdown = rescan_time
      start_countdown = false
    }

    // Check to see if the route's cost has changed significantly
    if (rescan_countdown == 0) {
      if (calc_route_cost / total_orig_cost > cost_ratio_threshold) {
        should_reroute = true
        rescan_countdown = rescan_time
        super.react()
      }
    }
    rescan_countdown -= 1
  }

  // TODO remember cost of route from A* instead of recalculating it
  private def calc_route_cost(): Double = {
    var cost = 0.0
    var eta = a.sim.tick
    for (r <- a.route.current_path) {
      // TODO broken now!!!
      cost += 0//r.to.intersection.tollbooth.toll
      eta += r.freeflow_time
    }
    return cost
  }
}
