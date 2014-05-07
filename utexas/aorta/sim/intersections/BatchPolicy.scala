// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.sim.Simulation
import utexas.aorta.sim.make.IntersectionType
import utexas.aorta.map.Vertex

import utexas.aorta.common.cfg

// Ignore ordering. Batch people if they're reasonably close, choose the batch based on proximity to
// intersection, and have some timeout.
class BatchPolicy(vertex: Vertex, sim: Simulation) extends Policy(vertex) {
  // TODO serialization

  private def candidates = request_queue.filter(ticket => !ticket.turn_blocked)

  private var started_at = 0.0
  private val timeout = 90.0

  // Only call if timeout shouldn't take over
  private def accept_to_current_batch() {
    var continue = true
    while (continue) {
      continue = false
      for (ticket <- candidates) {
        // Has to be compatible with all current people
        if (!accepted.exists(other => ticket.turn.conflicts_with(other.turn))) {
          // Has to be close enough to intersection
          // TODO when low, switches frequently. when high, doesnt let people go during overtime.
          if (ticket.a.at.dist_left <= 3 * ticket.turn.from.speed_limit * cfg.dt_s) {
            accept(ticket)
          }
        }
      }
    }
  }

  private def choose_new_batch() {
    if (candidates.nonEmpty) {
      // TODO also prefer most demand?
      accept(candidates.minBy(ticket => ticket.a.at.dist_left))
      started_at = sim.tick
    }
  }

  def react() {
    // Do we have a batch right now?
    if (accepted.isEmpty) {
      choose_new_batch()
    } else {
      // Accept more people, or timeout?
      if (sim.tick - started_at < timeout) {
        accept_to_current_batch()
      }
    }
  }

  def policy_type = IntersectionType.Batch
}
