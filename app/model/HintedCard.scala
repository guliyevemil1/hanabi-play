package model

import scala.collection.mutable
import model.Color.Color
import model.Number.Number

class HintedCard(val card : Card) {
  var hints = new mutable.HashSet[Hint]()

  private def justColorHints : mutable.HashSet[Hint] = {
    hints.filter(_.hint.isLeft)
  }

  private def justNumberHints : mutable.HashSet[Hint] = {
    hints.filter(_.hint.isRight)
  }

  def getColor : String = {
    justColorHints.toList.find(_.is).map(_.hint.left.get.toString).getOrElse("black")
  }

  def getNumber : String = {
    justNumberHints.toList.find(_.is).map(_.hint.right.get.toString).getOrElse("0")
  }

  private def negColorHints : Set[Hint] = {
    justColorHints.toSet.filterNot(_.is)
  }

  private def negNumberHints : Set[Hint] = {
    justNumberHints.toSet.filterNot(_.is)
  }

  private def findOtherColors : Set[Hint] = {
    val s = negColorHints

    if (s.size == 4) {
      val colors : Set[Color] = Color.values -- s.map(_.hint.left.get)
      colors.map(c => Hint(is = true, Left(c)))
    } else {
      s
    }
  }

  private def findOtherNumbers : Set[Hint] = {
    val s = negNumberHints

    if (s.size == 4) {
      val numbers : Set[Number] = Number.values -- s.map(_.hint.right.get)
      numbers.map(c => Hint(is = true, Right(c)))
    } else {
      s
    }
  }

  def addHint(hint : Hint): Unit = {
    if (hint.is) {
      if (hint.hint.isLeft) {
        hints = justNumberHints
      } else {
        hints = justColorHints
      }
      hints += hint
    } else {
      hints += hint
      if (hint.hint.isLeft) {
        hints = justNumberHints ++findOtherColors
      } else {
        hints = justColorHints ++findOtherNumbers
      }
    }
  }

  def getHints : List[Hint] = {
    hints.toList
  }
}
