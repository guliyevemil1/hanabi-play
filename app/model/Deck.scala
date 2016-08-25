package model

import scala.collection.mutable
import scala.util.Random

object Deck {
  private def cards(rainbow : Boolean): mutable.Stack[Card] = {
    val colors = if (rainbow) {
        Color.values
      } else {
        Color.values
      }
    val myCards = new mutable.Stack[Card]()
    for (c <- Color.values; i <- Number.nums) yield { myCards.push(Card(c, i)) }
    myCards
  }

  def shuffledDeck(rainbow : Boolean) : mutable.Stack[Card] = {
    Random.shuffle(Deck.cards(rainbow))
  }
}
