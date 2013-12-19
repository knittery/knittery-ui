package controllers

import scala.concurrent.duration._
import play.api.Play._
import play.api.mvc._
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

  def subscribe = TODO

}