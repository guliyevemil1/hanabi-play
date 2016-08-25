package model

import scala.collection.mutable
import model.Color.Color
import model.Number.Number

import scala.collection.mutable.ListBuffer

class HintedCard(val card : Card) {
  var colorHints = {
    val allHints = new mutable.HashSet[Color]()
    allHints ++= Color.values
    allHints
  }

  var numberHints = {
    val allHints = new mutable.HashSet[Number]()
    allHints ++= Number.values
    allHints
  }

  var hasColorHint = false
  var hasNumberHint = false

  def addHint(hint : Hint): Unit = {
    if (hint.is) {
      if (hint.hint.isLeft) {
        colorHints.clear()
        colorHints += hint.hint.left.get
      } else {
        numberHints.clear()
        numberHints += hint.hint.right.get
      }
    } else {
      if (hint.hint.isLeft) {
        colorHints -= hint.hint.left.get
      } else {
        numberHints -= hint.hint.right.get
      }
    }
  }

  def getColorHints : List[Hint] = {
    colorHints.map(c => Hint(is = true, Left(c))).toList
  }

  def getNumberHints : List[Hint] = {
    numberHints.toList.sortBy(_.id).map(n => Hint(is = true, Right(n)))
  }

  def getColor() : String = {
    if (colorHints.size == 1) {
      colorHints.head.toString
    } else {
      "black"
    }
  }

  def getNumber() : String = {
    if (numberHints.size == 1) {
      numberHints.head.toString
    } else {
      "0"
    }
  }
}
