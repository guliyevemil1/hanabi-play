package controllers

import model.{Board, HintType}
import model.HintType._

import scala.concurrent.Promise

private object BoardController {
  val ACTION : String = "action"
  val PLAY : String = "play"
  val DISCARD : String = "discard"
  val HINT_COLOR : String = "hintcolor"
  val HINT_NUMBER : String = "hintnumber"
  val INDEX : String = "idx"
  val HINTEE : String = "hintee"
}

class BoardController(val board : Board) {
  def reloadAllScreens() {
    board.getPlayers.foreach(_.notifyPlayer())
  }

  def play(m : Map[String, Seq[String]]) : Unit = {
    val i = m(BoardController.INDEX).head.toInt
    board.play(i)
  }

  def discard(m : Map[String, Seq[String]]) : Unit = {
    val i = m(BoardController.INDEX).head.toInt
    board.discard(i)
  }

  def hintColor(m : Map[String, Seq[String]]) : Unit = {
    val hintee = m(BoardController.HINTEE).head.toInt
    val hintType : HintType = HintType.Color
    val i = m(BoardController.INDEX).head.toInt
    board.hint(hintee, hintType, i)
  }

  def hintNumber(m : Map[String, Seq[String]]) : Unit = {
    val hintee = m(BoardController.HINTEE).head.toInt
    val hintType : HintType = HintType.Number
    val i = m(BoardController.INDEX).head.toInt
    board.hint(hintee, hintType, i)
  }

  def parseMove(m : Map[String, Seq[String]]) {
    m(BoardController.ACTION).head match {
      case BoardController.PLAY => play(m)
      case BoardController.DISCARD => discard(m)
      case BoardController.HINT_COLOR => hintColor(m)
      case BoardController.HINT_NUMBER => hintNumber(m)
    }
    reloadAllScreens()
  }

  def getPlayerPromise(idTokenString : String) : Option[Promise[String]] = {
    board.getPlayer(idTokenString).map(_.getPromise)
  }
}
