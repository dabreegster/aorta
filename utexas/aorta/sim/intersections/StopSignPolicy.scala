// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.sim.EV_IntersectionOutcome
import utexas.aorta.sim.make.IntersectionType
import utexas.aorta.map.Vertex

import utexas.aorta.common.cfg

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(vertex: Vertex, ordering: IntersectionOrdering[Ticket]) extends Policy(vertex)
{
  def react() {
    if (accepted.isEmpty) {
      approve_next()
    }
  }

  def policy_type = IntersectionType.StopSign

  // Agents must pause a moment, be the head of their queue, and be close enough
  // to us (in case they looked ahead over small edges).
  private def candidates =
    request_queue.filter(ticket =>
      (ticket.a.is_stopped &&
       ticket.a.cur_queue.head.get == ticket.a &&
       ticket.a.how_far_away(intersection) <= cfg.end_threshold &&
       !ticket.turn_blocked)
    )

  private def approve_next() {
    ordering.choose(candidates, request_queue, this) match {
      case Some(ticket) => {
        sim.publish(EV_IntersectionOutcome(
          policy_type, request_queue.filter(t => t.turn.from != ticket.turn.from).toList
        ))
        accept(ticket)
      }
      case None =>
    }
  }
}
