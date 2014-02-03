package controllers

import scala.concurrent.duration._
import play.api.Play._
import play.api.mvc._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.pattern.ask
import akka.util._
import models.guide.Guider

object Preview extends Controller {
  private implicit val timeout: Timeout = 100.millis
  private implicit def system = Akka.system
  protected def guider = system.actorSelection(Akka.system / "guider")

  def show = Action.async {
    for {
      Guider.CurrentStep(step) <- guider ? Guider.QueryStep
      finalState = step.last.stateAfter
      output = finalState.output
    } yield {
      Ok(views.html.preview(output.toString))
    }
  }

}