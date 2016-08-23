package model

import java.util.Stack

import scala.util.Random

object Deck {
  val cards: List[Card] = (for (c <- Color.values; i <- Number.nums) yield new Card(c, i)).toList;
  def shuffledDeck : Stack[Card] = {
    val deck = new Stack[Card]()
    Random.shuffle(Deck.cards).foreach(deck.push)
    deck
  }
}
