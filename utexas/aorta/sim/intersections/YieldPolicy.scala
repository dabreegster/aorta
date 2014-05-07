// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import scala.collection.mutable

import utexas.aorta.map.{Turn, Edge, Vertex}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.IntersectionType

import utexas.aorta.common.{Util, cfg}

// Automatically admit agents requesting common turns, and make the others queue and go when they
// can.
class YieldPolicy(vertex: Vertex, ordering: IntersectionOrdering[Ticket]) extends Policy(vertex) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  private val common_turns = compute_common_turns

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def react() {
    // First admit everybody trying to do a common turn, unless somebody rare and conflicting has
    // been approved.

    // Because we have to maintain turn invariants as we accept, do a fixpoint approach and accept
    // till there's nobody left that we can.
    // TODO ^ refactor this by making an abstract 'candidates' routine
    val candidates = new mutable.TreeSet[Ticket]()
    candidates ++= request_queue.filter(
      ticket => common_turns.contains(ticket.turn) && !rare_blocks(ticket.turn)
    )
    var changed = true
    while (changed && candidates.nonEmpty) {
      changed = false
      for (ticket <- candidates) {
        if (!ticket.turn_blocked) {
          // TODO refactor this "last chance before they'll slow down" predicate
          if (ticket.a.kinematic.max_lookahead_dist >= ticket.a.how_far_away(intersection) - cfg.end_threshold) {
            accept(ticket)
            candidates -= ticket
            changed = true
          }
        }
      }
    }

    // TODO how to use ordering to do diff stuff than from normal?
    // Now admit rare agents who're ready and wouldn't interrupt an accepted commoner. We don't need
    // to do a fixpoint here, since is_ready demands they be the head of their queue. We're really
    // not nice to rare turns; they've got to act like a stop sign.
    for (ticket <- request_queue if !ticket.turn_blocked) {
      if (!common_turns.contains(ticket.turn) && rare_is_ready(ticket.a) &&
          !commoner_blocks(ticket.turn) && !rare_blocks(ticket.turn))
      {
        accept(ticket)
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def policy_type = IntersectionType.Yield
  override def dump_info() {
    super.dump_info()
    Util.log(s"Common turns: $common_turns")
  }

  override def current_greens =
    if (accepted_rares.nonEmpty)
      super.current_greens
    else
      common_turns

  private def accepted_commoners() =
    accepted.filter(t => common_turns.contains(t.turn))
  private def accepted_rares() =
    accepted.filter(t => !common_turns.contains(t.turn))

  private def commoner_blocks(turn: Turn) =
    accepted_commoners.exists(t => t.turn.conflicts_with(turn))
  private def rare_blocks(turn: Turn) =
    accepted_rares.exists(t => t.turn.conflicts_with(turn))

  // Rare agents must be the head of their queue and be close enough to us (in case they looked
    // ahead over small edges).
  private def rare_is_ready(a: Agent) =
    (a.cur_queue.head.get == a &&
     a.how_far_away(intersection) <= cfg.end_threshold)

  // Turns leading between two major roads are good, and none may conflict
  // Note that if all roads at the intersection are major, then the turns picked here might be
  // terrible
  private def compute_common_turns(): Set[Turn] = {
    val best = vertex.turns.filter(t => t.from.road.is_major && t.to.road.is_major)
    // Arbitrary order may be bad...
    val result = new mutable.HashSet[Turn]()
    for (candidate <- best) {
      if (!result.exists(t => t.conflicts_with(candidate))) {
        result += candidate
      }
    }
    return result.toSet
  }
}
