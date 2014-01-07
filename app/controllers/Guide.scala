package controllers

import scala.concurrent.Future
import scala.concurrent.duration._
import java.awt.Color
import scalaz._
import Scalaz._
import akka.actor._
import akka.util._
import akka.pattern.ask
import play.api.Play._
import play.api.mvc._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import models._
import models.plan._
import models.planners._
import models.guide._
import utils._
import java.util.UUID

object Guide extends Controller {
  private implicit val timeout: Timeout = 100.millis

  protected def guider = Akka.system.actorSelection("akka://application/user/guider")

  def view = Action.async {
    for {
      Guider.CurrentStep(step) <- guider ? Guider.QueryStep
    } yield Ok(views.html.guide(step))
  }

  def next = Action.async { request =>
    for {
      Guider.CommandExecuted(_) <- guider ? Guider.Next
    } yield Redirect(routes.Guide.view)
  }
  def previous = Action.async { request =>
    for {
      Guider.CommandExecuted(_) <- guider ? Guider.Previous
    } yield Redirect(routes.Guide.view)
  }

}