package controllers

import javax.inject._

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.Materializer
import model.HintType
import model.HintType.HintType
import model.Board
import play.api._
import play.api.libs.streams.ActorFlow
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Promise}

private object HanabiController {
  val board : Board = new Board(2, false)

  val ACTION : String = "action"
  val PLAY : String = "play"
  val DISCARD : String = "discard"
  val HINT_COLOR : String = "hintcolor"
  val HINT_NUMBER : String = "hintnumber"
  val INDEX : String = "idx"
  val HINTEE : String = "hintee"
}


class HanabiController @Inject() (implicit system: ActorSystem,
                                  materializer: Materializer) extends Controller {
  var nextMove = Promise[String]()

  class ReloadActor(out : ActorRef, val nextMove : Promise[String]) extends Actor {
    override def receive : Receive = {
      case msg : String => nextMove.future.onComplete(post => out ! "reload")(ExecutionContext.global)
    }
  }

  def reload = WebSocket.accept[String, String] { _ =>
    ActorFlow.actorRef(out => {
      Props(new ReloadActor(out, nextMove))
    })
  }

  def board = {
    Action {Ok(HanabiController.board.toString)}
  }

  def boardHtml(id : Int) = {
    Action {Ok(views.html.board(HanabiController.board, id))}
  }

  def play(m : Map[String, Seq[String]]) : Unit = {
    val i = m(HanabiController.INDEX).head.toInt
    HanabiController.board.play(i)
  }

  def discard(m : Map[String, Seq[String]]) : Unit = {
    val i = m(HanabiController.INDEX).head.toInt
    HanabiController.board.discard(i)
  }

  def hintColor(m : Map[String, Seq[String]]) : Unit = {
    val hintee = m(HanabiController.HINTEE).head.toInt
    val hintType : HintType = HintType.Color
    val i = m(HanabiController.INDEX).head.toInt
    HanabiController.board.hint(hintee, hintType, i)
  }

  def hintNumber(m : Map[String, Seq[String]]) : Unit = {
    val hintee = m(HanabiController.HINTEE).head.toInt
    val hintType : HintType = HintType.Number
    val i = m(HanabiController.INDEX).head.toInt
    HanabiController.board.hint(hintee, hintType, i)
  }

  def parseMove(m : Map[String, Seq[String]]) {
    m(HanabiController.ACTION).head match {
      case HanabiController.PLAY => play(m)
      case HanabiController.DISCARD => discard(m)
      case HanabiController.HINT_COLOR => hintColor(m)
      case HanabiController.HINT_NUMBER => hintNumber(m)
    }
  }

  def move = Action { implicit request =>
    nextMove.success("reload")
    nextMove = Promise[String]()
    val json : Option[Map[String, Seq[String]]] = request.body.asFormUrlEncoded
    json.foreach(parseMove)
    Ok("")
  }
}
