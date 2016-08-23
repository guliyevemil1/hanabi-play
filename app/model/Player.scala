package model

import scala.collection.mutable

class Player(val idx : Int) {
  var name : String = s"P$idx"
  val hand = new mutable.ListBuffer[HintedCard]

  def getHand : List[HintedCard] = {
    hand.toList
  }

  def setName(newName : String): Unit = {
    name = newName
  }

  def setShowable(v : Boolean): Unit = {
    hand foreach(card => card.card.showable = v)
  }
}