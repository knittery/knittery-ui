package controllers

import scala.concurrent.Future
import scala.concurrent.duration._
import scalaz._
import Scalaz._
import akka.util._
import akka.pattern.ask
import play.api.Play._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import models.guide._
import utils._
import JsonSerialization._

object Guide extends Controller {
  private implicit val timeout: Timeout = 100.millis
  private implicit def system = Akka.system
  protected def guider = system.actorSelection(Akka.system / "guider")

  def view = Action.async {
    for {
      Guider.CurrentStep(step, instruction, steps) <- guider ? Guider.QueryStep
    } yield Ok(views.html.guide(steps, step, instruction))
  }

  private def execute(command: Guider.Command) = Action.async { request =>
    (guider ? command) map (_ match {
      case Guider.CommandExecuted(_) => Ok("executed")
      case Guider.CommandNotExecuted(_, reason) => InternalServerError(reason)
    })
  }

  def next = execute(Guider.NextStep)
  def previous = execute(Guider.PreviousStep)
  def first = execute(Guider.First)
  def last = execute(Guider.Last)
  def jumpTo(step: Int, instruction: Int) = execute(Guider.JumpTo(step, instruction))
  def nextInstruction = execute(Guider.NextInstruction)
  def previousInstruction = execute(Guider.PreviousInstruction)

  def subscribe = WebSocket.async[JsValue] { implicit request =>
    val loc = localized
    import loc._
    for {
      actor <- guider.resolveOne()
      Guider.CurrentStep(step, instruction, steps) <- actor ? Guider.QueryStep
      e = ActorEnumerator.enumerator(Guider.subscription(actor))
      fst = Enumerator[Any](Guider.ChangeEvent(step, instruction))
      json = (fst >>> e) &> Enumeratee.collect {
        case Guider.ChangeEvent(step, instruction) => Json.obj(
          "event" -> "change",
          "step" -> step,
          "instruction" -> instruction,
          "planInfo" -> Json.obj(
            "totalSteps" -> steps.size,
            "totalInstructions" -> steps.flatMap(_.instructions).size
          )): JsValue
      }
    } yield (Iteratee.ignore, json)
  }
}