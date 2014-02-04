package controllers

import scala.concurrent.duration._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.predefined._
import play.api.Play._
import play.api.mvc._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.pattern.ask
import akka.util._
import models.guide._
import models.plan._
import scala.concurrent.Future
import net.liftweb.json.JString

object Preview extends Controller {
  private implicit val timeout: Timeout = 100.millis
  private implicit def system = Akka.system
  protected def guider = system.actorSelection(Akka.system / "guider")

  def show = Action {
    Ok(views.html.preview())
  }

  def json = GuiderAction { req =>
    val graph = req.finalGraph
    val alias = graph.nodes.zipWithIndex.map {
      case (node, index) => node.value -> s"s$index"
    }.toMap
    val stitchDescriptor = new NodeDescriptor[Stitch2](typeId = "stitch") {
      def id(node: Any) = node match {
        case s: Stitch2 => alias(s)
      }
      override def decompose(node: Any) = node match {
        case s: Stitch2 => JString(alias(s))
      }
    }
    val json = graph.toJson(new Descriptor(
      defaultNodeDescriptor = stitchDescriptor,
      defaultEdgeDescriptor = WUnDi.descriptor()))
    Ok(json).as("application/json")
  }

  def dot = GuiderAction { req =>
    val graph = req.finalGraph
    val alias = graph.nodes.zipWithIndex.map {
      case (node, index) => node.value -> s"n-$index"
    }.toMap
    val root = DotRootGraph(directed = false, id = None)
    def trans(e: Graph[Stitch2, WUnDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
      Some((root, DotEdgeStmt(alias(e.edge._1.value), alias(e.edge._2.value), Nil)))
    }
    val dot = graph.toDot(root, trans _)
    Ok(dot).as("text/vnd.graphviz")
  }

  class RequestWithStep[A](val step: GuideStep, request: Request[A]) extends WrappedRequest[A](request) {
    def finalState = step.last.stateAfter
    def finalGraph = finalState.output2.asGraph
  }
  case object GuiderAction extends ActionBuilder[RequestWithStep] {
    protected override def invokeBlock[A](request: Request[A], block: (RequestWithStep[A]) ⇒ Future[SimpleResult]) = {
      for {
        Guider.CurrentStep(step) <- guider ? Guider.QueryStep
        req = new RequestWithStep(step, request)
        result <- block(req)
      } yield result
    }
  }

}