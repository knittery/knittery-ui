package controllers

import scala.concurrent.duration._
import play.api.Play._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.pattern.ask
import akka.util.Timeout
import models.machine.Machine._
import utils.ActorEnumerator
import utils.JsonSerialization._
import models.machine.Machine

object Display extends Controller {
  protected implicit val system = Akka.system
  protected def machine = system.actorSelection(system / "machine")
  protected implicit val timeout: Timeout = 2.seconds

  def show = Action {
    Ok(views.html.display())
  }

  def subscribe = WebSocket.async[JsValue] { req =>
    val r = machine.resolveOne
    val pos = machine ? GetPositions
    val pat = machine ? GetNeedlePattern
    for {
      machineRef <- r
      Positions(data) <- pos
      needlePattern @ NeedlePatternUpdate(_) <- pat
      e = ActorEnumerator.enumerator(Machine.subscription(machineRef))
      fst = Enumerator.enumerate[Any](data.map(d => PositionChanged(d._1, d._2))) >>>
        Enumerator[Any](needlePattern)
      json = Enumeratee.collect[Any] {
        case event: PositionChanged => Json.toJson(event)
        case NeedlePatternUpdate(row) =>
          Json.toJson(Json.obj(
            "event" -> "needlePatternUpdate",
            "patternRow" -> row.andThen(_.toPosition)))
      }
    } yield (Iteratee.ignore, fst >>> e &> json)
  }
}