// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.sim.drivers.Wallet
import utexas.aorta.sim.make.{WalletType, SystemWalletConfig}

// Just used for bookkeeping... how much is some system bid affecting things?
class SystemWallet() extends Wallet(0, 0) {
  override def toString = "SYS"
  def wallet_type = WalletType.System

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) = Nil
  def bid_signal(phases: Iterable[Phase], ours: Ticket) = Nil
  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) = Nil

  // Infinite budget.
  override def spend(amount: Int, ticket: Ticket) {}
}

// Bids to maintain "fairness."
class SystemWallets(rates: SystemWalletConfig) {
  // Keep these separate just for book-keeping
  val thruput = new SystemWallet()
  val capacity = new SystemWallet()
  val dependency = new SystemWallet()
  val waiting = new SystemWallet()
  val ready = new SystemWallet()

  def meta_bid[T](items: List[T], policy: Policy): List[Bid[T]] =
    (bid_thruput(items, policy) ++ bid_pointless_impatience(items, policy) ++
     bid_dependency(items, policy) ++ bid_waiting(items, policy) ++
     bid_ready(items, policy))

  // Promote bids that don't conflict
  def bid_thruput[T](items: List[T], policy: Policy) = policy match {
    case p: ReservationPolicy =>
      for (ticket <- items if p.can_accept(ticket.asInstanceOf[Ticket]))
        yield Bid(thruput, ticket, rates.thruput_bonus, null)

    case _ => Nil
  }

  // Reward individual tickets that aren't trying to rush into a queue already
  // filled to some percentage of its capacity
  def bid_pointless_impatience[T](items: List[T], policy: Policy) = policy match
  {
    // TODO maybe look at all target queues for the phase?
    case _: SignalPolicy => Nil
    case _ => items.flatMap(ticket => {
      val target = ticket.asInstanceOf[Ticket].turn.to.queue
      if (target.percent_avail >= rates.avail_capacity_threshold)
        Some(Bid(capacity, ticket, rates.capacity_bonus, null))
      else
        None
    })
  }

  // Reward the lane with the most people.
  // TODO for full queues, recursing to find ALL dependency would be cool.
  def bid_dependency[T](items: List[T], policy: Policy) = policy match {
    case p: SignalPolicy =>
      for (phase <- items)
        yield Bid(dependency, phase, rates.dependency_rate * num_phase(phase), null)
    case _ =>
      for (ticket <- items)
        yield Bid(dependency, ticket, rates.dependency_rate * num_ticket(ticket), null)
  }
  private def num_phase(phase: Any) =
    phase.asInstanceOf[Phase].turns.map(_.from.queue.agents.size).sum
  /*private def num_ticket(ticket: Any) =
    ticket.asInstanceOf[Ticket].a.cur_queue.agents.size*/
  // Just bid for the head of the queue, aka, for multi-auction
  // reservations, just start things right.
  private def num_ticket(ticket: Any): Int = {
    val t = ticket.asInstanceOf[Ticket]
    return if (t.a.cur_queue.head.get == t.a)
      t.turn.from.queue.agents.size
    else
      0
  }

  // Help drivers who've been waiting the longest.
  def bid_waiting[T](items: List[T], policy: Policy) = policy match {
    case p: SignalPolicy =>
      for (phase <- items)
        yield Bid(dependency, phase, (rates.waiting_rate * waiting_phase(phase)).toInt, null)
    case _ =>
      for (ticket <- items)
        yield Bid(
          dependency, ticket,
          (rates.waiting_rate * ticket.asInstanceOf[Ticket].how_long_waiting).toInt, null
        )
  }
  private def waiting_phase(phase: Any) =
    phase.asInstanceOf[Phase].all_tickets.map(_.how_long_waiting).sum

  // Promote bids of agents close enough to usefully start the turn immediately
  def bid_ready[T](items: List[T], policy: Policy) = policy match {
    case p: ReservationPolicy =>
      for (ticket <- items if ticket.asInstanceOf[Ticket].close_to_start)
        yield Bid(ready, ticket, rates.ready_bonus, null)

    case _ => Nil
  }
}
