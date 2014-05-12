// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.sim.{Simulation, EV_IntersectionOutcome}
import utexas.aorta.sim.make.IntersectionType
import utexas.aorta.map.Vertex

import utexas.aorta.common.{Util, StateWriter, StateReader, TurnID}

// Accept as many compatible turns as possible, until an interruption occurs.
// (To get the old greedy behavior, add the constraint back to candidates, or
// reimplement it with an ordering with "inertia.")
class ReservationPolicy(vertex: Vertex, ordering: IntersectionOrdering[Ticket])
  extends Policy(vertex)
{
  // Prevent more from being accepted until this ticket is approved.
  private var interruption: Option[Ticket] = None

  override def serialize(w: StateWriter) {
    super.serialize(w)
    interruption match {
      case Some(ticket) => {
        w.ints(ticket.a.id.int, ticket.turn.id.int)
      }
      case None => {
        w.ints(-1, -1)
      }
    }
  }

  override protected def unserialize(r: StateReader, sim: Simulation) {
    val agent_id = r.int
    val turn_id = new TurnID(r.int)
    if (agent_id != -1) {
      interruption = Some(Policy.find_ticket(sim, agent_id, turn_id))
    }
  }

  // TODO and are reasonably close? otherwise somebody who looks ahead a tick
  // earlier than another gets an advantage, even if theyre really far away
  // TODO thats why waiting a bit to accept turns makes sense.. get more people involved.
  protected def candidates = request_queue.filter(ticket => !ticket.turn_blocked)
  def can_accept(ticket: Ticket) = !accepted.exists(t => t.turn.conflicts_with(ticket.turn))

  def react(): Unit = {
    // TODO keep accepting people that dont conflict with interruption?
    interruption match {
      case Some(ticket) => {
        // Can we admit them now?
        if (can_accept(ticket)) {
          // Yes! Resume admitting others now, too.
          ticket.approve()
          accepted += ticket
          interruption = None
        } else {
          // Not yet, and don't let anyone else in either.
          return
        }
      }
      case None =>
    }

    // Approve candidates as long as there are candidates.
    while (!interruption.isDefined) {
      ordering.choose(candidates, request_queue, this) match {
        case Some(ticket) => {
          sim.publish(EV_IntersectionOutcome(
            policy_type, request_queue.filter(t => t.turn.conflicts(ticket.turn)).toList
          ))
          // Admit them immediately and continue, or reserve an interruption?
          if (can_accept(ticket)) {
            accept(ticket)
          } else {
            interruption = Some(ticket)
            ticket.is_interruption = true
            unqueue(ticket)
            // Furthermore, grab a spot for them and keep it!
            ticket.turn.to.queue.allocate_slot()
            return
          }
        }
        case None => return
      }
    }
  }

  override def cancel_turn(ticket: Ticket) {
    interruption match {
      case Some(t) if t == ticket => {
        interruption = None
      }
      case _ => super.cancel_turn(ticket)
    }
  }

  override def dump_info() {
    super.dump_info()
    Util.log(s"Interruption by $interruption")
  }
  def policy_type = IntersectionType.Reservation
}
