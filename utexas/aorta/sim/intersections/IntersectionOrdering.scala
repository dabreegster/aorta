// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.sim.drivers.{Agent, Wallet}
import utexas.aorta.sim.make.{IntersectionType, OrderingType}

import scala.collection.mutable

import utexas.aorta.common.{Util, cfg, VertexID}

abstract class IntersectionOrdering[T <: Ordered[T]]() {
  def choose(choices: Iterable[T], participants: Iterable[Ticket], client: Policy): Option[T]
  def ordering_type(): OrderingType.Value
}

class FIFO_Ordering[T <: Ordered[T]]() extends IntersectionOrdering[T]() {
  def ordering_type = OrderingType.FIFO
  def choose(choices: Iterable[T], participants: Iterable[Ticket], client: Policy): Option[T] = {
    // Reset tooltips for GUI...
    participants.foreach(t => t.a.wallet.reset_tooltip)
    return choices.headOption
  }
}

// Who's willing to pay how much for what underlying purpose. Purpose will be
// null for system bids.
case class Bid[T](who: Wallet, item: T, amount: Int, purpose: Ticket)
{
  Util.assert_ge(amount, 0.0)
  override def toString = s"Bid($amount for $item)"
}

class AuctionOrdering[T <: Ordered[T]]() extends IntersectionOrdering[T]() {
  def ordering_type = OrderingType.Auction
  def choose(choices: Iterable[T], participants: Iterable[Ticket], client: Policy): Option[T] = {
    // Reset tooltips for GUI...
    participants.foreach(t => t.a.wallet.reset_tooltip)

    // Handle degenerate cases where we don't hold auctions.
    if (choices.isEmpty) {
      return None
    }
    if (choices.size == 1) {
      return Some(choices.head)
    }

    // Collect bids, remembering what each agent bids, and group by choice.
    val bids = new mutable.HashMap[T, mutable.Set[Bid[T]]] with mutable.MultiMap[T, Bid[T]]
    val multipliers = new mutable.HashMap[T, Int]()
    choices.foreach(item => multipliers(item) = 1)
    participants.foreach(who => {
      for ((ticket, amount) <- who.a.wallet.bid(choices, who, client)) {
        bids.addBinding(ticket, Bid(who.a.wallet, ticket, amount, who))
      }
    })
    // Ask the System, too, interpreting responses as multipliers to existing
    // bids
    for (bid <- SystemWallets.meta_bid(choices.toList, client) if bid.amount > 0) {
      multipliers(bid.item) *= bid.amount
      // and implicitly add 1 unit of currency to the user bid, so the system
      // can help freeriders.
      bids.addBinding(bid.item, Bid(bid.who, bid.item, 1, null))
    }

    return if (bids.isEmpty) {
      // They're all apathetic, so just do FIFO.
      return choices.headOption
    } else {
      val debug = client.intersection.v.id == new VertexID(-1)
      Some(process_auction(debug, bids, multipliers, client))
    }
  }

  private def process_auction(debug: Boolean, bids: mutable.MultiMap[T, Bid[T]], multipliers: mutable.HashMap[T, Int], client: Policy): T =
  {
    // Break ties arbitrarily but deterministically.
    // The .toList is necessary, since it's a set otherwise... same bids get
    // squished together!
    val sums_by_item: List[(T, Int)] = bids.keys.map(
      t => (t, bids(t).toList.map(bid => bid.amount).sum * multipliers(t))
    ).toList.sortBy(pair => pair._1)
    if (sums_by_item.tail.isEmpty) {
      // Actually, just one choice. Freebie!
      return sums_by_item.head._1
    }

    if (debug) {
      for (i <- sums_by_item) {
        println(s"... ${i._2} (multiplier ${multipliers(i._1)}) for ${i._1}")
      }
      println("")
    }

    // Now sort by sum. As long as this is a stable sort, fine.
    // (Lotsa type nonsense when I try to do this in one go.)
    val sums = sums_by_item.sortBy(pair => pair._2).reverse

    val winner = sums.head._1

    client.policy_type match {
      case IntersectionType.Reservation => {
        val winner_ticket = winner.asInstanceOf[Ticket]
        // The one winning group pays the bid of the highest losing group who
        // had a conflicting turn.
        sums.tail.find(
          pair => pair.asInstanceOf[(Ticket, Int)]._1.turn.conflicts_with(winner_ticket.turn)
        ) match {
          case Some(group) => {
            pay(bids(winner).toList, multipliers(winner), group._2)
          }
          // If nobody's turn conflicts, then don't pay at all!
          case None =>
        }
      }
      case _ => {
        pay(bids(winner).toList, multipliers(winner), sums.tail.head._2)
      }
    }

    return winner
  }

  // A group splits some cost somehow.
  private def pay(who: List[Bid[T]], multiplier: Int, total_runnerup: Int) {
    // Proportional payment... Each member of the winner pays proportional to
    // what they bid.
    val total_winners = who.map(_.amount).sum * multiplier
    Util.assert_ge(total_winners, total_runnerup)
    // Pay what the runner up totalled, but let the same system rate that
    // boosted our total divide theirs.
    val total_cost = total_runnerup.toDouble / multiplier.toDouble
    val our_orig_total = total_winners.toDouble / multiplier.toDouble
    val rate = total_cost / our_orig_total
    for (bid <- who) {
      val price = (bid.amount * rate).toInt
      assert(price <= bid.amount)
      bid.who.spend(price, bid.purpose)

      // Disabled because the string concat is super expensive, and this isn't that useful right now
      /*// Since wallets may override spend, it's easier to just stick this
      // here...
      bid.who.tooltip = List(price + " of " + bid.who.tooltip.mkString(", "))
      bid.who.dark_tooltip = true*/
    }
  }
}
