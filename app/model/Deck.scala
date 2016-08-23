package model

import scala.collection.mutable
import scala.util.Random

object Deck {
  private def cards: mutable.Stack[Card] = {
    val myCards = new mutable.Stack[Card]()
    for (c <- Color.values; i <- Number.nums) yield { myCards.push(Card(c, i)) }
    myCards
  }

  def shuffledDeck : mutable.Stack[Card] = {
    Random.shuffle(Deck.cards)
  }
}
