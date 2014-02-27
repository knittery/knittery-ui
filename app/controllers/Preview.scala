package controllers

import java.awt.Color
import scala.concurrent.Future
import scala.concurrent.duration._
import scalax.collection.Graph
import scalax.collection.edge.WLUnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import play.api.Play._
import play.api.mvc._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import akka.pattern.ask
import akka.util._
import models.Yarn
import models.guide._
import models.plan._
import utils.graph._
import utils.JsonSerialization._

object Preview extends Controller {
  private implicit val timeout: Timeout = 100.millis
  private implicit def system = Akka.system
  protected def guider = system.actorSelection(Akka.system / "guider")

  def show = Action {
    Ok(views.html.preview())
  }

  private def colorRgb(color: Color) = {
    val value = color.getRGB | 0xff000000
    "#" + value.toHexString.drop(2)
  }

  def json = GuiderAction { req =>
    val graph = req.finalGraph
    val alias = graph.nodes.zipWithIndex.map {
      case (node, index) => node.value -> s"s$index"
    }.toMap

    //var layout = SpringLayout(graph, Box(2000))
    //var layout = SpringBarnesHutLayout(graph, Box(2000))
    var layout = ImmutableSpringLayout(graph, Box(2000))

    println(s"Prelayouting ${graph.size} nodes...")
    var i = 0
    val t = System.currentTimeMillis
    val layoutingSteps = 3000
    val layoutingMax = 10.minutes
    while (i < 5000 && System.currentTimeMillis - t < layoutingMax.toMillis) {
      layout = layout.improve()
      i = i + 1
      if (i % 200 == 0) println(s"  layout step $i of $layoutingSteps (after ${(System.currentTimeMillis - t) / 1000}s)")
    }
    val duration = System.currentTimeMillis - t
    println(s"Performance: ${(duration * 1000 / i).round} us per iteration ($i iterations).")

    val nodeJson = graph.nodes.map { node =>
      val yarns = node.value.points.map(_.yarn).toSet
      Json.obj(
        "id" -> alias(node),
        "colors" -> yarns.map(_.color).map(colorRgb),
        "position" -> layout(node))
    }
    val edgeJson = graph.edges.map { edge =>
      val color = edge.label match { case Yarn(_, color) => color }
      Json.obj(
        "n1" -> alias(edge._1),
        "n2" -> alias(edge._2),
        "weight" -> edge.weight,
        "color" -> colorRgb(color))
    }
    val json = Json.obj(
      "nodes" -> nodeJson,
      "edges" -> edgeJson)
    Ok(json)
  }

  def dot = GuiderAction { req =>
    val graph = req.finalGraph
    val alias = graph.nodes.zipWithIndex.map {
      case (node, index) => node.value -> s"n-$index"
    }.toMap
    val root = DotRootGraph(directed = false, id = None)
    def trans(e: Graph[Stitch2, WLUnDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
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