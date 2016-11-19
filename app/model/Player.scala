package model

import scala.collection.mutable
import scala.concurrent.Promise

class Player(val user : User, val idx : Int) {
  val hand = new mutable.ListBuffer[HintedCard]

  def getName = user.name

  def getFullName = user.fullName

  def getPicture = user.picture

  def getHand : List[HintedCard] = {
    hand.toList
  }

  def setShowable(v : Boolean): Unit = {
    hand foreach(card => card.card.showable = v)
  }

  private var nextMove : Promise[String] = Promise[String]()

  def getPromise = nextMove

  def notifyPlayer() {
    nextMove.success("reload")
    nextMove = Promise[String]()
  }
}