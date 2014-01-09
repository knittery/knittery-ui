package controllers

import scala.concurrent.duration._
import play.api.Play._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.iteratee._
import Concurrent.Channel
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import models._
import models.machine.Machine._
import utils.JsonSerialization._
import utils.SubscriptionActor
import utils.SubscribeFailed
import utils.ActorEnumerator
import models.machine.Machine

object Display extends Controller {
  protected implicit val system = Akka.system
  protected def machine = system.actorSelection(system / "machine")
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

  def subscribe = WebSocket.async[JsValue] { req =>
    val r = machine.resolveOne
    val pos = machine ? GetPositions
    val pat = machine ? GetNeedlePattern
    for {
      machineRef <- r
      Positions(data, row) <- pos
      needlePattern @ NeedlePatternUpdate(_, _) <- pat
      e = ActorEnumerator.enumerator(Machine.subscription(machineRef))
      fst = Enumerator.enumerate[Any](data.map(d => PositionChanged(d._1, d._2, row))) >>>
        Enumerator[Any](needlePattern)
      json = Enumeratee.collect[Any] {
        case event: PositionChanged => Json.toJson(event)
        case NeedlePatternUpdate(row, _) =>
          Json.toJson(Json.obj(
            "event" -> "needlePatternUpdate",
            "patternRow" -> row))
      }
    } yield (Iteratee.ignore, fst >>> e &> json)
  }
}