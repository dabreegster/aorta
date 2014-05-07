// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.map.{Turn, Vertex, Road, Traversable}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.intersections.Ticket
import utexas.aorta.sim.make.{MkIntersection, IntersectionType, WalletType, RouterType}
import utexas.aorta.analysis.RerouteCountMonitor
import utexas.aorta.common.Util

sealed trait Sim_Event

final case class EV_Step(tick: Double) extends Sim_Event
// Logged every 1.0 real-time seconds. Number of agent_steps and paths found are since the last
// heartbeat; this is one of the few things tracked online.  Active agents is the number that moved
// the specific tick that this heartbeat was taken.
final case class EV_Heartbeat(
  active_agents: Int, live_agents: Int, spawning_agents: Int, done_agents: Int, tick: Double,
  agent_steps: Int, counts: RerouteCountMonitor
) extends Sim_Event {
  //private val agent_str = "%,d moved, %,d live, %,d ready, %,d done"
  private val agent_str = "A[%,d] L[%,d] R[%,d] D[%,d]"
  def describe_agents = agent_str.format(active_agents, live_agents, spawning_agents, done_agents)
  def describe = "At t=%s: %s {%,03d moves, %,d A* (%,dm + %,df)}".format(
    Util.time_num(tick), describe_agents, agent_steps, counts.astar_count,
    counts.unrealizable_count, counts.discretionary_count
  )
}
// The GUI should pause and drop into a REPL
final case class EV_Breakpoint(focus: Agent) extends Sim_Event

final case class EV_Signal_Change(greens: Set[Turn]) extends Sim_Event
final case class EV_IntersectionOutcome(intersection: IntersectionType.Value, losers: List[Ticket])
  extends Sim_Event
final case class EV_TurnFinished(
  agent: Agent, vert: Vertex, req_tick: Double, accept_tick: Double, done_tick: Double,
  cost_paid: Double
) extends Sim_Event {
  // Total delay means turn length factors in.
  def total_delay = done_tick - req_tick
  def accept_delay = accept_tick - req_tick
}
final case class EV_TurnApproved(ticket: Ticket) extends Sim_Event
final case class EV_TurnStarted(ticket: Ticket) extends Sim_Event

final case class EV_AgentSpawned(a: Agent) extends Sim_Event
final case class EV_AgentQuit(
  agent: Agent, birth_tick: Double, start: Road, end: Road, wallet: WalletType.Value,
  start_budget: Int, end_tick: Double, end_budget: Int, priority: Double
) extends Sim_Event
{
  // Note this includes waiting to actually spawn on the first lane
  def trip_time = end_tick - birth_tick
  def total_spent = start_budget - end_budget
}

final case class EV_Transition(a: Agent, from: Traversable, to: Traversable) extends Sim_Event
// orig = true when initializing the path. bit of a hack.
// if unrealizable, then couldn't follow path. if not, congestion or gridlock.
final case class EV_Reroute(
  a: Agent, path: List[Road], orig: Boolean, method: RouterType.Value, unrealizable: Boolean,
  old_path: List[Road]
) extends Sim_Event
final case class EV_LinkChanged(r: Road, congested: Boolean) extends Sim_Event
