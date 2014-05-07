// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import utexas.aorta.sim.{EV_Transition, EV_Reroute}
import utexas.aorta.sim.make.{WalletType, IntersectionType, OrderingType, SystemWalletConfig}
import utexas.aorta.map.{Turn, Vertex}
import utexas.aorta.sim.intersections.{Ticket, Policy, Phase}

import utexas.aorta.common.{Util, cfg, StateReader, StateWriter, Serializable}

// Express an agent's preferences of trading between time and cost.
abstract class Wallet(initial_budget: Int, val priority: Double) extends Serializable {
  //////////////////////////////////////////////////////////////////////////////
  // Transient state

  protected var debug_me = false

  //////////////////////////////////////////////////////////////////////////////
  // State

  protected var owner: Agent = null

  // How much the agent may spend during its one-trip lifetime
  var budget = initial_budget

  // How much is this agent willing to spend on some choice?
  var tooltip: List[String] = Nil
  // Dark indicates they won and paid.
  var dark_tooltip = false

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    // TODO initial budget vs current... will it matter?
    w.ints(wallet_type.id, budget)
    w.double(priority)
    w.int(tooltip.size)
    tooltip.foreach(line => w.string(line))
    w.bool(dark_tooltip)
  }

  def setup(a: Agent) {
    owner = a
  }

  protected def unserialize(r: StateReader) {}
  
  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def spend(amount: Int, ticket: Ticket) {
    Util.assert_ge(budget, amount)
    budget -= amount
    ticket.cost_paid += amount
  }

  def reset_tooltip() {
    tooltip = Nil
    dark_tooltip = false
  }

  def set_debug(value: Boolean) {
    debug_me = value
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def wallet_type(): WalletType.Value

  def bid[T](choices: Iterable[T], ours: Ticket, policy: Policy): Iterable[(T, Int)] = {
    val result = policy.policy_type match {
      case IntersectionType.StopSign =>
        bid_stop_sign(
          choices.asInstanceOf[Iterable[Ticket]], ours
        ).asInstanceOf[Iterable[(T, Int)]]
      case IntersectionType.Signal =>
        bid_signal(
          choices.asInstanceOf[Iterable[Phase]], ours
        ).asInstanceOf[Iterable[(T, Int)]]
      case IntersectionType.Reservation =>
        bid_reservation(
          choices.asInstanceOf[Iterable[Ticket]], ours
        ).asInstanceOf[Iterable[(T, Int)]]
      case _ => throw new Exception(s"Dunno how to bid on $policy")
    }
    // TODO ideally not the latest bid, but the one for the prev turn or
    // something. also, hard to represent what we're bidding for each thing...
    // If we bid for multiple items, just show the different prices.
    tooltip = result.map(_._2.toString).toSet.toList
    return result
  }
  protected def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): Iterable[(Ticket, Int)]
  protected def bid_signal(phases: Iterable[Phase], ours: Ticket): Iterable[(Phase, Int)]
  protected def bid_reservation(tickets: Iterable[Ticket], ours: Ticket): Iterable[(Ticket, Int)]

  // This is for just our ticket.
  protected def just_my_ticket(tickets: Iterable[Ticket], ours: Ticket) =
    tickets.filter(t => t == ours)
  // Pay for people ahead of us.
  protected def greedy_my_ticket(tickets: Iterable[Ticket], ours: Ticket) =
    tickets.filter(t => t.turn.from == ours.turn.from)
  // Return all that match, wind up just paying for one if it wins.
  protected def my_phases(phases: Iterable[Phase], ours: Ticket) =
    phases.filter(p => p.has(ours.turn))
}

object Wallet {
  def unserialize(r: StateReader): Wallet = {
    // Subclasses that care about bid_ahead will unserialize it.
    val wallet = WalletType.make(WalletType(r.int), r.int, r.double, false)
    wallet.tooltip = Range(0, r.int).map(_ => r.string).toList
    wallet.dark_tooltip = r.bool
    wallet.unserialize(r)
    return wallet
  }
}

// Always bid the full budget, but never lose any money.
class StaticWallet(initial_budget: Int, p: Double)
  extends Wallet(initial_budget, p)
{
  //////////////////////////////////////////////////////////////////////////////
  // Actions

  override def spend(amount: Int, ticket: Ticket) {
    Util.assert_ge(budget, amount)
    ticket.cost_paid = amount
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = s"STATIC $budget"
  def wallet_type = WalletType.Static

  private def bid_full[T](choice: Iterable[T]) =
    choice.map(thing => (thing, budget))

  // Be greedier. We have infinite budget, so contribute to our queue.
  override def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    bid_full(greedy_my_ticket(tickets, ours))

  override def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    bid_full(my_phases(phases, ours))

  override def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    bid_full(greedy_my_ticket(tickets, ours))
}

// Never participate.
class FreeriderWallet(p: Double) extends Wallet(0, p) {
  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = "FR"
  def wallet_type = WalletType.Freerider

  override def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) = Nil
  override def bid_signal(phases: Iterable[Phase], ours: Ticket) = Nil
  override def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) = Nil
}

// Bid once per intersection some amount proportional to the rest of the trip.
class FairWallet(initial_budget: Int, p: Double, initial_bid_ahead: Boolean)
  extends Wallet(initial_budget, p)
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  private var total_weight = 0.0
  private var bid_ahead = initial_bid_ahead

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    w.double(total_weight)
    w.bool(bid_ahead)
  }

  override protected def unserialize(r: StateReader) {
    total_weight = r.double
    bid_ahead = r.bool
  }

  override def setup(agent: Agent) {
    super.setup(agent)
    agent.sim.listen(classOf[EV_Reroute], owner, _ match {
      case ev: EV_Reroute => total_weight = ev.path.map(r => weight(r.to)).sum
    })
    agent.sim.listen(classOf[EV_Transition], owner, _ match {
      case EV_Transition(a, from, to: Turn) => total_weight -= weight(to.vert)
      case _ =>
    })
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = s"FAIR $budget"
  def wallet_type = WalletType.Fair

  protected def my_ticket(tickets: Iterable[Ticket], ours: Ticket) =
    if (bid_ahead)
      greedy_my_ticket(tickets, ours)
    else
      just_my_ticket(tickets, ours)

  private def bid_fair[T](choice: Iterable[T], v: Vertex) = choice.map(
    thing => (thing, math.floor(budget * (weight(v) / total_weight)).toInt)
  )

  override def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    bid_fair(my_ticket(tickets, ours), ours.turn.vert)

  override def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    bid_fair(my_phases(phases, ours), ours.turn.vert)

  override def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    bid_fair(my_ticket(tickets, ours), ours.turn.vert)

  // How much should we plan on spending, or actually spend, at this
  // intersection?
  // TODO better heuristic? base it on the policy, too!
  private def weight(v: Vertex) = 1.0
}
