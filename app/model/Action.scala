package model

import Color._
import Number._

sealed trait Action

case class Discard(player : Player, card : Card) extends Action
case class Play(player : Player, card : Card) extends Action
case class GiveHint(hinter : Player, hintee : Player, hint : Either[Color, Number]) extends Action