package controllers

import scala.concurrent.duration._
import play.api.Play._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import models._
import models.Machine._
import utils.JsonSerialization._

object Display extends Controller {

  protected def machine = Akka.system.actorSelection("akka://application/user/machine")
  protected implicit val timeout: Timeout = 2.seconds

  def show = Action {
    Ok(views.html.display())
  }

  def positions = Action.async {
    val pos = machine ? GetPositions
    val pat = machine ? GetNeedlePattern
    for {
      position <- pos
      pattern <- pat
    } yield (position, pattern) match {
      case (Positions(data, row), NeedlePatternUpdate(patternRow, _)) =>
        val positions = JsObject(data.map {
          case (c, p) => Json.toJson(c).as[String] -> Json.toJson(p)
        }.toSeq)
        Ok(Json.obj(
          "positions" -> positions,
          "row" -> row,
          "patternRow" -> patternRow))
      case _ => InternalServerError("Machine actor did not respond.")
    }
  }

  def subscribe = WebSocket.using[JsValue] { req =>
    (Iteratee.ignore, Subscription.enumerator)
  }

  object Subscription {
    def enumerator = e

    private val (e, channel) = Concurrent.broadcast[JsValue]
    Akka.system.actorOf(Props(new SubscriptionActor), "display-subscription")

    class SubscriptionActor extends Actor with ActorLogging {
      override def preStart = {
        log.debug("Subscribing to machine")
        machine ! Subscribe
      }
      override def postStop = {
        machine ! Unsubscribe
      }
      override def receive = {
        case Subscribed =>
          log.debug("Subscription established")
          context become subscribed(sender)
      }
      def subscribed(to: ActorRef): Receive = {
        case event: PositionChanged =>
          channel push Json.toJson(event)
        case NeedlePatternUpdate(row, _) =>
          channel push Json.toJson(Json.obj(
            "event" -> "needlePatternUpdate",
            "patternRow" -> row))
        case Terminated if sender == to =>
          log.debug(s"Resubscribing, $sender has crashed")
          machine ! Subscribe
          context become receive
      }
    }
  }
}