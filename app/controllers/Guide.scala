package controllers

import scala.concurrent.duration._
import java.net.URI
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
import play.utils.UriEncoding

object Guide extends Controller {
  private implicit val timeout: Timeout = 100.millis
  private implicit def system = Akka.system
  protected def guider = system.actorSelection(Akka.system / "guider")

  private implicit object localWikiResolver extends WikiResolver {
    override def resolve(id: String) = {
      val path = UriEncoding.encodePathSegment(id, "UTF-8")
      URI.create(s"http://localhost/ms/wiki/$path.html")
    }
  }

  def view = Action.async {
    for {
      Guider.CurrentStep(step, instruction, steps) <- guider ? Guider.QueryStep
    } yield Ok(views.html.guide(steps, step, instruction))
  }

  def next = Action.async { request =>
    for {
      Guider.CommandExecuted(_) <- guider ? Guider.NextStep
    } yield Redirect(routes.Guide.view)
  }
  def previous = Action.async { request =>
    for {
      Guider.CommandExecuted(_) <- guider ? Guider.PreviousStep
    } yield Redirect(routes.Guide.view)
  }
  def first = Action.async { request =>
    for {
      Guider.CommandExecuted(_) <- guider ? Guider.First
    } yield Redirect(routes.Guide.view)
  }
  def last = Action.async { request =>
    for {
      Guider.CommandExecuted(_) <- guider ? Guider.Last
    } yield Redirect(routes.Guide.view)
  }

  def nextInstruction = Action.async { request =>
    for {
      Guider.CommandExecuted(_) <- guider ? Guider.NextInstruction
    } yield Redirect(routes.Guide.view)
  }
  def previousInstruction = Action.async { request =>
    for {
      Guider.CommandExecuted(_) <- guider ? Guider.PreviousInstruction
    } yield Redirect(routes.Guide.view)
  }

  def subscribe = WebSocket.async[JsValue] { implicit request =>
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