package model

import Color._
import Number._

object HintType extends Enumeration {
  type HintType = Value
  val Color, Number = Value
}

case class Hint(is : Boolean, hint : Either[Color, Number]) {
  private val stringValue = mkString

  private def mkString : String = {
    val sb = new StringBuilder
    if (!is) {
      sb.append("not ")
    }
    sb.append(hint.fold(_.toString, _.toString))
    sb.mkString
  }

  override def toString : String = stringValue
}