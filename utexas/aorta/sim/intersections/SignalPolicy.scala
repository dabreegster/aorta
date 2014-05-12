// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import scala.annotation.tailrec
import scala.collection.mutable
import Function.tupled

import utexas.aorta.map.{Turn, Vertex, Edge}
import utexas.aorta.sim.{EV_Signal_Change, Simulation, EV_IntersectionOutcome, Queue}
import utexas.aorta.sim.make.{IntersectionType, OrderingType}

import utexas.aorta.common.{Util, cfg, StateWriter, StateReader}

// A phase-based light.
class SignalPolicy(vertex: Vertex, ordering: IntersectionOrdering[Phase]) extends Policy(vertex) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  // phase_order maintains the list of all possible phases, in order of LRU
  var phase_order = new mutable.ListBuffer[Phase]()
  phase_order ++= setup_phases
  private var current_phase = phase_order.head
  phase_order = phase_order.tail ++ List(current_phase)

  // Tracks when the current phase began
  private var started_at = 0.0

  // TODO maybe get rid of this after I'm done improving it
  private var overtime_total = 0.0

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    w.double(started_at)
    phase_order.foreach(p => w.int(p.id))
  }

  override protected def unserialize(r: StateReader, sim: Simulation) {
    started_at = r.double
    // Learn our phases
    val phases = phase_order.map(p => p.id -> p).toMap
    phase_order.clear()
    phase_order ++= Range(0, phases.size).map(_ => phases(r.int))
    current_phase = phase_order.last
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def react() {
    // Switch to the next phase
    if (sim.tick >= end_at && accepted.isEmpty) {
      overtime_total += sim.tick - end_at
      // In auctions, we may not have a viable next phase at all...
      ordering.choose(candidates, request_queue, this) match {
        case Some(p) => {
          sim.publish(
            EV_IntersectionOutcome(policy_type, request_queue.filter(t => !p.has(t.turn)).toList)
          )
          current_phase = p
          phase_order = phase_order.filter(phase => phase != p)
          phase_order += p

          started_at = sim.tick

          sim.publish(EV_Signal_Change(current_phase.turns.toSet))
        }
        case None =>  // shouldn't happen...
      }
    }

    // Accept new agents into the current phase
    if (!in_overtime) {
      // Because we have to maintain turn invariants as we accept, do a fixpoint
      // approach and accept till there's nobody left that we can.
      val candidates_now = new mutable.TreeSet[Ticket]()
      candidates_now ++= request_queue.filter(
        ticket => current_phase.has(ticket.turn) && could_make_light(ticket)
      )
      var changed = true
      while (changed && candidates_now.nonEmpty) {
        changed = false
        for (ticket <- candidates_now) {
          if (!ticket.turn_blocked) {
            accept(ticket)
            candidates_now -= ticket
            changed = true
          }
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def policy_type = IntersectionType.Signal
  override def dump_info() {
    super.dump_info()
    Util.log(s"Current phase: $current_phase")
    Util.log(s"${phase_order.size} phases total")
    Util.log("Time left: " + time_left)
    Util.log("Viable phases right now: " + candidates)
    Util.log(s"Overtime: $overtime_total")
  }

  // If we're holding auctions, we risk agents repeatedly bidding for and
  // winning a phase that doesn't have any agents that can actually go yet.
  // Avoid that. In the FIFO case, pretend we're not autonomous and pick useless
  // phases.
  def candidates =
    if (ordering.ordering_type == OrderingType.FIFO)
      phase_order
    else
      phase_order.filter(p => p.all_tickets.exists(t => !t.turn_blocked))

  override def current_greens =
    if (in_overtime)
      Set() // nobody should go right now
    else
      current_phase.turns.toSet

  // Ideally, ignoring overtime for slow agents.
  private def end_at = started_at + current_phase.duration
  // May be negative if we're in overtime
  private def time_left = end_at - sim.tick
  private def in_overtime = sim.tick > end_at

  private def setup_phases(): List[Phase] = {
    val phase_ls = Phase.phases_for(vertex)

    val turns_seen = new mutable.HashSet[Turn]
    var expect_offset = phase_ls.head.offset
    for (phase <- phase_ls) {
      Util.assert_eq(phase.offset, expect_offset)
      expect_offset += phase.duration
      turns_seen ++= phase.turns
    }
    Util.assert_eq(turns_seen.size, vertex.turns.size)

    return phase_ls
  }

  // If we admit agents that run up overtime, that's safe but adds to our delay
  // and makes the light unfair.
  private def could_make_light(ticket: Ticket): Boolean = {
    if (ticket.a.is_stopped) {
      if (ticket.a.our_lead.isDefined) {
        // Wait till they start moving.
        return false
      } else {
        // (almost) always let first go
        return ticket.earliest_finish < time_left
      }
    } else {
      // Defer decision till we're close and have to choose.
      // TODO this might be over-conservative (tends to minimize overtime but stagger agents
      // starting from rest)
      if (ticket.a.kinematic.max_lookahead_dist >= ticket.a.how_far_away(intersection) - cfg.end_threshold) {
        val dist_left = ticket.a.how_far_away(intersection) + ticket.turn.length
        return ticket.a.our_lead match {
          // We're bounded by them either way, so...
          case Some(other) => dist_left / other.speed < time_left
          // We can speed up soon
          case None => dist_left / ticket.a.kinematic.max_next_speed < time_left
        }
      } else {
        return false
      }
    }
  }
}

class Phase(val id: Int, val turns: Set[Turn], val offset: Double, val duration: Double)
  extends Ordered[Phase]
{
  // Can only order phases at one intersection! Offsets must be unique!
  override def compare(other: Phase) = offset.compare(other.offset)
  //override def toString = s"Phase($id, $turns, offset $offset, duration $duration)"
  override def toString = s"Phase $id"

  Util.assert_ge(offset, 0)
  Util.assert_gt(duration, 0)
  for (t1 <- turns; t2 <- turns if t1 < t2) {
    Util.assert_eq(t1.conflicts_with(t2), false)
  }

  def has(turn: Turn) = turns(turn)

  def all_agents = turns.flatMap(_.from.queue.all_agents).toSet
  def head_agents = turns.map(t => t.from.queue).toSet.flatMap((q: Queue) => q.head)
  def all_tickets: Set[Ticket] = {
    val i = turns.head.vert.intersection
    return all_agents.flatMap(a => a.all_tickets(i)).filter(t => has(t.turn))
  }
}

object Phase {
  def phases_for(v: Vertex): List[Phase] = {
    // Arbitrary grouping
    //return phase_maker(i.v, (remaining: Iterable[Turn], start: Turn) => remaining)

    // Try to have turns in the same and opposite (antiparallel) direction grouped
    return phase_maker(v, (remaining: Iterable[Turn], start: Turn) => {
      val pair = remaining.partition(t => parallel(t, start))
      pair._1.toList ++ pair._2.toList
    })
  }

  // Assign the same arbitrary duration to everything
  private def turn_groups_to_phases(groups: List[Set[Turn]]) =
    // TODO duration has to let agents have time to cross the intersection at a
    // reasonable speed
    groups.zipWithIndex.map(
      tupled((turns, idx) => new Phase(idx, turns, idx * cfg.signal_duration, cfg.signal_duration))
    )

  // Add turns to every group that don't conflict
  private def maximize_groups(groups: List[Set[Turn]], turns: List[Turn]) =
    groups.map(g => maximize(g, turns))
  @tailrec private def maximize(group: Set[Turn], turns: List[Turn]): Set[Turn] = turns match {
    case Nil => group
    case turn :: rest if !group.exists(t => t.conflicts_with(turn)) => maximize(group + turn, rest)
    case turn :: rest => maximize(group, rest)
  }

  // Least number of phases can be modeled as graph coloring, but we're just
  // going to do a simple greedy approach.
  private def phase_maker(vert: Vertex, order: (Iterable[Turn], Turn) => Iterable[Turn]): List[Phase] = {
    val turns_remaining = new mutable.TreeSet[Turn]()
    turns_remaining ++= vert.turns

    val groups = new mutable.ListBuffer[Set[Turn]]()
    while (turns_remaining.nonEmpty) {
      val this_group = new mutable.HashSet[Turn]()
      this_group += turns_remaining.head
      turns_remaining -= this_group.head

      // conflict relation is symmetric, but not transitive... so do quadratic
      // conflict checking
      for (candidate <- order(turns_remaining, this_group.head)) {
        if (!this_group.exists(t => t.conflicts_with(candidate))) {
          this_group += candidate
          turns_remaining -= candidate
        }
      }

      groups += this_group.toSet
    }
    return turn_groups_to_phases(maximize_groups(groups.toList, vert.turns))
  }

  // Threshold of 10 degrees
  private def parallel(t1: Turn, t2: Turn) =
    math.abs(canonicalize_angle(t1.angle_deg) - canonicalize_angle(t2.angle_deg)) <= 10.0
  private def canonicalize_angle(degrees: Double) =
    if (degrees < 180)
      degrees
    else
      degrees - 180
}
