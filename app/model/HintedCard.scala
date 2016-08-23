package model

import scala.collection.mutable

class HintedCard(val card : Card) {
  val hints = new mutable.HashSet[Hint]()

  def addHint(hint : Hint): Unit = {
    hints += hint
  }

  def getHints : List[Hint] = {
    hints.toList
  }
}
