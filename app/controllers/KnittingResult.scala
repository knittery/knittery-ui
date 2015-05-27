package controllers

import java.awt.Color
import scala.concurrent.Future
import scala.concurrent.duration._
import scalax.collection.io.dot._
import play.api.Play._
import play.api.mvc._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import akka.pattern.ask
import akka.util._
import ch.inventsoft.graph.layout.{Layout, LayoutOps}
import ch.inventsoft.graph.vector.Box3
import models.Yarn
import models.guide._
import models.plan._
import utils.JsonSerialization._

object KnittingResult extends Controller {
  private implicit val timeout: Timeout = 100.millis
  private implicit def system = Akka.system
  protected def guider = system.actorSelection(Akka.system / "guider")

  def state = Action.async {
    for {
      Guider.FinalState(state) <- guider ? Guider.GetFinalState
      json = Json.obj("state" -> state)
    } yield Ok(json)
  }

  def mainBed = Action.async {
    for {
      Guider.FinalState(state) <- guider ? Guider.GetFinalState
      json = Json.obj("output" -> state.output.mainBed.rows)
    } yield Ok(json)
  }

  def doubleBed = Action.async {
    for {
      Guider.FinalState(state) <- guider ? Guider.GetFinalState
      json = Json.obj("output" -> state.output.doubleBed.rows)
    } yield Ok(json)
  }

  private def colorRgb(color: Color) = {
    val value = color.getRGB | 0xff000000
    "#" + value.toHexString.drop(2)
  }
}
