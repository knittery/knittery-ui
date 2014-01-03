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
  private def system = Akka.system
  private def actorName(id: UUID) = "guider-" + id.toString

  class RequestWithGuider[A](val guider: ActorRef, request: Request[A]) extends WrappedRequest[A](request)
  case class GuiderAction(id: UUID) extends ActionBuilder[RequestWithGuider] {
    protected override def invokeBlock[A](request: Request[A], block: (RequestWithGuider[A]) ⇒ Future[SimpleResult]) = {
      val actor = system.actorSelection(system / actorName(id))
      actor.resolveOne(100.millis).
        map(Some(_)).recover { case _ => None }.
        flatMap {
          case Some(guider) => block(new RequestWithGuider(guider, request))
          case None => Future.successful(Redirect(routes.Guide.start))
        }
    }
  }

  def start = Action {
    val id = UUID.randomUUID
    val guider = Akka.system.actorOf(Guider.props(plan), actorName(id))
    Redirect(routes.Guide.execute(id))
  }

  def execute(id: UUID) = GuiderAction(id).async { request =>
    for {
      Guider.CurrentStep(step) <- request.guider ? Guider.QueryStep
    } yield Ok(views.html.guide(step, id))
  }

  def next(id: UUID) = GuiderAction(id).async { request =>
    for {
      Guider.CommandExecuted(_) <- request.guider ? Guider.Next
    } yield Redirect(routes.Guide.execute(id))
  }
  def previous(id: UUID) = GuiderAction(id).async { request =>
    for {
      Guider.CommandExecuted(_) <- request.guider ? Guider.Previous
    } yield Redirect(routes.Guide.execute(id))
  }

  private val plan = {
    val yarn1 = Yarn("red", Color.red)
    val yarn2 = Yarn("blue", Color.blue)
    def checkerboard(w: Int, h: Int): Matrix[Yarn] = {
      val s = Stream.from(0).map(i => if (i % 2 == 0) yarn1 else yarn2)
      (0 until h).map { i =>
        val c = if (i % 2 == 0) s else s.drop(1)
        c.take(w).toIndexedSeq
      }
    }
    val width = 40
    val height = 20
    val planner = Cast.onClosed(Needle.atIndex(100 - width / 2), Needle.atIndex(100 + width / 2), yarn1) >>
      FairIslePlanner.singleBed(checkerboard(Needle.count, height), yarn1) >>
      Cast.offClosed(yarn1)
    planner.plan.toOption.get
  }
}