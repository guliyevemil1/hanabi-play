package model

import scala.collection.mutable.ListBuffer

@SerialVersionUID(6822161743322278255L)
class BoardLounge(val uuid : String, val leader : User) extends Serializable {
  private var players : ListBuffer[User] = ListBuffer()

  def canStart : Boolean = {
    2 <= players.length && players.length <= 4
  }

  def addPlayer(player : User) {
    if (players.indexOf(player) < 0) {
      players += player
    }
  }

  def getPlayerCount = players.length

  def createBoard(): Option[Board] = Some(board).filter(_ => canStart)

  private lazy val board : Board = {
    new Board(uuid, players, false)
  }
}
