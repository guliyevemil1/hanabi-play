package model

import scala.collection.mutable
import Color.Color
import Number.Number
import model.HintType.HintType
import play.api.libs.json._

import scala.collection.mutable.ListBuffer

class Board(playerCount : Int) {
  private val deck = Deck.shuffledDeck

  private val history = new mutable.Stack[Action]()
  private val discarded = new mutable.Stack[Card]()
  private val piles = new mutable.HashMap[Color, Number]()
  private var postDeckTurns = 0

  private var turn : Int = 0
  private var bombs : Int = 3
  private var hints : Int = 8

  private val players : List[Player] = {
    val playersTmp = new ListBuffer[Player]()
    for (i <- 0 until playerCount) {
      playersTmp += newPlayer(i)
    }
    playersTmp.toList
  }

  def getPiles : Map[Color, Number] = {
    piles.toMap
  }

  def getPlayers : List[Player] = {
    players
  }

  def getDiscarded : List[Card] = {
    discarded.toList
  }

  def getHistory : List[Action] = {
    history.toList
  }

  def getDeckSize : Int = {
    deck.size()
  }

  def getHintCount : Int = {
    hints
  }

  def getBombCount : Int = {
    bombs
  }

  def getTurn : Int = {
    turn
  }

  private def cardCount() : Int = {
    playerCount match {
      case 2 => 5
      case 3 => 5
      case 4 => 4
      case 5 => 4
      case _ => throw new Exception("Invalid number of players")
    }
  }

  private def newPlayer(i : Int) : Player = {
    val player : Player = new Player(i)
    (1 to cardCount) foreach { _ => player.hand += drawCard() }
    player
  }

  private def pilesToJson() : JsObject = {
    JsObject(piles map { case (c, n) => (c.toString, JsString(n.toString)) })
  }

  private def showPlayers(playerIdx : Int) : List[Player] = {
    players.foreach(player => player.setShowable(player.idx != currentTurn))
    players
  }

  private def toJson(playerIdx : Int) : JsValue = {
    JsObject(Seq(
                  "turn" -> JsNumber(turn),
                  "bombs" -> JsNumber(bombs),
                  "hints" -> JsNumber(hints),
                  "post-deck-turns" -> JsNumber(postDeckTurns),
                  "piles" -> pilesToJson(),
                  "discard" -> JsArray(discarded.map(Json.toJson(_))),
                  "players" -> JsArray(showPlayers(playerIdx).map(Json.toJson(_))),
                  "history" -> JsArray(history.map(Json.toJson(_)))
                ))
  }

  private implicit val cardWrites = new Writes[Card] {
    override def writes(card : Card) : JsValue = {
      if (card.showable) {
        Json.obj(
                  "color" -> card.color.toString,
                  "number" -> card.number.toString
                )
      } else {
        Json.obj(
                  "color" -> "black",
                  "number" -> "0"
                )
      }
    }
  }

  private implicit val hintHintWrites = new Writes[Either[Color, Number]] {
    override def writes(hint : Either[Color, Number]) : JsValue = {
      val (hintType, value) = hint match {
        case Left(c) => ("color", c.toString)
        case Right(n) => ("number", n.toString)
      }
      JsObject(Seq("type" -> JsString(hintType), "value" -> JsString(value)))
    }
  }

  private implicit val hintWrites = new Writes[Hint] {
    override def writes(hint : Hint) : JsValue = {
      Json.obj(
                "is" -> JsBoolean(hint.is),
                "hint" -> Json.toJson(hint.hint)
              )
    }
  }

  private implicit val hintedCardWrites = new Writes[HintedCard] {
    override def writes(hintedCard : HintedCard) : JsValue = {
      Json.obj(
                "card" -> Json.toJson(hintedCard.card),
                "hints" -> hintedCard.hints.map(Json.toJson(_))
              )
    }
  }

  private implicit val playerWrites = new Writes[Player] {
    override def writes(player : Player) : JsValue = {
      Json.obj(
                "id" -> player.idx,
                "hand" -> player.hand.toList.map(Json.toJson(_))
              )
    }
  }

  private implicit val actionWrites = new Writes[Action] {
    override def writes(action : Action) : JsValue = action match {
      case Play(p, c) => JsObject(Seq(
                      "player" -> JsNumber(p.idx),
                      "card" -> Json.toJson(c)
                    ))
      case Discard(p, c) => JsObject(Seq(
                      "player" -> JsNumber(p.idx),
                      "card" -> Json.toJson(c)
                    ))
      case GiveHint(p1, p2, h) => JsObject(Seq(
                      "hinter" -> JsNumber(p1.idx),
                      "hintee" -> JsNumber(p2.idx),
                      "hint" -> Json.toJson(h)
                    ))
    }
  }


  override def toString : String = {
    toJson(currentTurn).toString
  }

  private def currentTurn : Int = {
    turn % playerCount
  }

  private def currentPlayer : Player = {
    players(currentTurn)
  }

  private def updateTurn() : Unit = {
    if (deck.empty()) {
      postDeckTurns += 1
    }
    turn += 1
  }

  private def playable(card : Card) : Boolean = {
    piles.get(card.color) == Number.previous(card.number)
  }

  private def drawCard() : HintedCard = {
    new HintedCard(deck.pop())
  }

  private def removeCardAndDraw(cardIndex : Int) : Card = {
    val removedCard = currentPlayer.hand.remove(cardIndex).card
    currentPlayer.hand += drawCard()
    removedCard
  }

  private def maxScore() : Int = {
    25
  }

  private def score() = {
    piles.values.map(v => v.id + 1).sum
  }

  private def gameIsOver() : Boolean = {
    bombs <= 0 || score >= maxScore || postDeckTurns >= playerCount
  }

  private def addHint() {
    if (hints >= 8) {
      hints = 8
    } else {
      hints += 1
    }
  }

  private def removeHint() {
    if (hints <= 0) {
      hints = 0
    } else {
      hints -= 1
    }
  }

  private def removeBomb() {
    if (bombs <= 0) {
      bombs = 0
    } else {
      bombs -= 1
    }
  }

  def play(cardIndex : Int) : Unit = {
    if (gameIsOver()) {
      return
    }

    var card : Card = removeCardAndDraw(cardIndex)

    if (playable(card)) {
      if (card.number == Number.Five) {
        addHint()
      }
      piles.put(card.color, card.number)
    } else {
      removeBomb()
      discarded.push(card)
    }

    history.push(Play(currentPlayer, card))

    updateTurn()
  }

  def discard(cardIndex : Int) : Unit = {
    if (gameIsOver()) {
      return
    }

    var card : Card = removeCardAndDraw(cardIndex)
    addHint()
    discarded.push(card)

    history.push(Discard(currentPlayer, card))

    updateTurn()
  }

  def hint(hintee : Int, hintType : HintType, cardIndex : Int) : Unit = {
    if (gameIsOver()) {
      return
    }

    val hinteeHand = players(hintee).hand

    val hint = hintType match {
      case HintType.Color => Left(hinteeHand(cardIndex).card.color)
      case HintType.Number => Right(hinteeHand(cardIndex).card.number)
    }

    hinteeHand.foreach(hintedCard => {
      val myValue = hintType match {
        case HintType.Color => Left(hintedCard.card.color)
        case HintType.Number => Right(hintedCard.card.number)
      }
      hintedCard.hints += Hint(hint == myValue, hint)
    })

    history.push(GiveHint(currentPlayer, players(hintee), hint))

    updateTurn()
  }
}
