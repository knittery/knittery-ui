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

object Display extends Controller {

  protected def machine = Akka.system.actorSelection("akka://application/user/machine")
  protected implicit val timeout: Timeout = 2.seconds

  def show = Action.async {
    (machine ? GetPositions).map {
      case Positions(data) =>
        val kPos = data.get(KCarriage)
        val lPos = data.get(LCarriage)
        val gPos = data.get(GCarriage)
        Ok(views.html.display(kPos, lPos, gPos))
      case _ => InternalServerError("Machine actor did not respond.")
    }
  }

  def subscribe = WebSocket.using[JsValue] { req =>
    val jsE = Subscription.enumerator &> Enumeratee.map {
      case PositionChanged(carriage, pos) =>
        val c = carriage match {
          case KCarriage => "K"
          case LCarriage => "L"
          case GCarriage => "G"
        }
        val p = pos match {
          case CarriageLeft(i) => Json.obj("where" -> "left", "overlap" -> i)
          case CarriageRight(i) => Json.obj("where" -> "right", "overlap" -> i)
          case CarriageOverNeedles(i) => Json.obj("where" -> "needles", "needle" -> i.number, "index" -> i.index)
        }
        val json: JsValue = Json.obj(
          "event" -> "positionChange", "carriage" -> c, "position" -> p)
        json
    }
    (Iteratee.ignore, jsE)
  }

  object Subscription {
    def enumerator = e

    private val (e, channel) = Concurrent.broadcast[PositionChanged]
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
          channel push event
        case Terminated if sender == to =>
          log.debug(s"Resubscribing, $sender has crashed")
          machine ! Subscribe
          context become receive
      }
    }
  }
}