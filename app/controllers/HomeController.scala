package controllers

import controllers.admin._
import java.util
import javax.inject._

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.Materializer
import com.google.api.client.googleapis.auth.oauth2.GoogleIdTokenVerifier
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import model.{Board, BoardLounge, User}
import play.api._
import play.api.cache._
import play.api.libs.streams.ActorFlow
import play.api.mvc._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(implicit cache : CacheApi,
                               cfg : Configuration,
                               system: ActorSystem,
                               materialer : Materializer) extends Controller {

  val clientId = cfg.getString("google.clientid")
    .getOrElse("467110528915-9b9a1a14e0hp1r0r3klh5krc9gadl1ku.apps.googleusercontent.com")
  val issuer = "accounts.google.com"
  val transport = GoogleNetHttpTransport.newTrustedTransport()
  val jsonFactory = JacksonFactory.getDefaultInstance()
  val idTokenVerifier : GoogleIdTokenVerifier = new GoogleIdTokenVerifier.Builder(transport, jsonFactory)
    .setAudience(util.Arrays.asList(clientId))
    .setIssuer(issuer)
    .build()

  val ACCT = "acct"

  /**
    * Create an Action to render an HTML page with a welcome message.
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index = withValidSignin((_,_) => Redirect("/mainmenu", 303))

  def signInStatus(acctMaybe : Option[Cookie]) : SignInStatus = {
    acctMaybe match {
      case None => NotSignedIn()
      case Some(acct) => {
        {
          val maybeUser : Option[User] = cache.get("idtoken:" + acct.value)
          maybeUser match {
            case None => InvalidSignin()
            case Some(user) => ValidSignin(user)
          }
        }
      }
    }
  }

  def hasInvalidStatus(request : RequestHeader) : Either[Result, User] = {
    val maybeCookie : Option[Cookie] = request.cookies.get(ACCT)
    signInStatus(maybeCookie) match {
      case NotSignedIn() => Left(Redirect("/signin", 303))
      case InvalidSignin() => Left(Redirect("/signin", 303))
      case ValidSignin(user) => Right(user)
    }
  }

  def withValidSignin(resultFunction : Function2[User, Request[AnyContent], Result]) = Action { implicit request =>
    hasInvalidStatus(request) match {
      case Left(err) => err
      case Right(user) => resultFunction(user, request)
    }
  }

  def signin = Action {
    Ok(views.html.signin(clientId))
  }

  def mainMenu = withValidSignin((_, _) => Ok(views.html.main()))

  def verify = Action { implicit request =>
    val json : Option[Map[String, Seq[String]]] = request.body.asFormUrlEncoded
    val idTokenString : String = json.get("id-token").head

    Option(idTokenVerifier.verify(idTokenString)) match {
      case Some(idToken) => {
        val payload = idToken.getPayload
        val name : String = payload.get("given_name").asInstanceOf[String]
        val fullName : String = payload.get("name").asInstanceOf[String]
        val pictureUrl : String = payload.get("picture").asInstanceOf[String]
        cache.set("idtoken:" + idTokenString, new User(idTokenString, name, fullName, pictureUrl))
        Ok("1").withCookies(Cookie("acct", idTokenString))
      }
      case None => Ok("0")
    }
  }

  def createGame = withValidSignin((user, request) => {
    val uuid = java.util.UUID.randomUUID.toString
    cache.set("boardlounge:" + uuid, new BoardLounge(uuid, user))
    Redirect("/boardlounge/" + uuid, 303)
  })

  def boardLounge(uuid : String) = withValidSignin((user, request) => {
    val maybeBoardLounge : Option[BoardLounge] = cache.get("boardlounge:" + uuid)

    if (maybeBoardLounge.isEmpty) {
      NotFound("No game with this uuid exists.")
    } else {
      val boardLounge = maybeBoardLounge.get
      boardLounge.addPlayer(user)
      Ok(views.html.boardlounge(boardLounge.getPlayerCount,
                                 user.idToken == boardLounge.leader.idToken && boardLounge.canStart,
                                 "/start/" + uuid
                               ))
    }
  })

  def startGame(uuid : String) = withValidSignin((user, request) => {
    val maybeBoardLounge : Option[BoardLounge] = cache.get("boardlounge:" + uuid)
    maybeBoardLounge match {
      case None => NotFound("No game with this uuid exists.")
      case Some(boardLounge) => {
        {
          if (user.idToken == boardLounge.leader.idToken) {
            val maybeBoard : Option[Board] = boardLounge.createBoard()
            maybeBoard match {
              case None => BadRequest("Can't start game yet.")
              case Some(board) => {
                {
                  val boardController : BoardController = new BoardController(board)
                  cache.set("board:" + board.uuid, boardController)
                  addToCurrentGames(uuid)
                  Redirect("/board/" + uuid, 303)
                }
              }
            }
          } else {
            BadRequest("You are not the leader of the board lounge.")
          }
        }
      }
    }
  })

  private def addToCurrentGames(uuid : String) : Unit = {
    val currentGames : mutable.Set[String] = cache.get("currentgames") match {
      case None => mutable.Set()
      case Some(set) => set.asInstanceOf[mutable.Set[String]]
    }
    currentGames += uuid
    cache.set("currentgames", currentGames)
  }

  def boardHtml(uuid : String) = Action { implicit request =>
    val maybeBoardController : Option[BoardController] = cache.get("board:" + uuid)
    maybeBoardController match {
      case None => NotFound("No game with this uuid exists.")
      case Some(boardController) => {
        {
          val board = boardController.board
          val maybeCookie : Option[Cookie] = request.cookies.get("acct")
          val maybeId = maybeCookie.map(_.value).map(board.getPlayerId)
          maybeId match {
            case None => Unauthorized("You are not in this game")
            case Some(id) => Ok(views.html.board(board, id))
          }
        }
      }
    }
  }

  val UUID : String = "uuid"

  def move = withValidSignin((user, request) => {
    val json : Option[Map[String, Seq[String]]] = request.body.asFormUrlEncoded
    val result : Option[Unit] = json.flatMap(m => {
      m.get(UUID).flatMap(uuid => cache.get[BoardController]("board:" + uuid.head)).map(_.parseMove(m))
    })
    result match {
      case None => BadRequest
      case Some(_) => Ok("")
    }
  })

  class ReloadActor(out : ActorRef, val nextMove : Promise[String]) extends Actor {
    override def receive : Receive = {
      case msg : String => nextMove.future.onComplete(post => out ! "reload")(ExecutionContext.global)
    }
  }

  def reload = WebSocket.acceptOrResult[String, String] { request : RequestHeader =>
    val gameDetails : Map[String, Seq[String]] = request.queryString
    val boardControllerMaybe : Option[BoardController] = gameDetails
      .get("uuid")
      .map(_.head)
      .flatMap(uuid => cache.get("board:" + uuid))
    val playerMaybe : Option[String] = gameDetails
      .get("idtoken")
      .map(_.head)
    val promiseMaybe = playerMaybe.flatMap(player => boardControllerMaybe.flatMap(
      boardController => boardController.getPlayerPromise(player)
                                                                                 ))
    Future.successful(promiseMaybe match {
      case None => Left(BadRequest)
      case Some(promise) => Right(ActorFlow.actorRef(out => {
        Props(new ReloadActor(out, promise))
      }))
    })
  }
}
