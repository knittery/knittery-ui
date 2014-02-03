package controllers

import scala.concurrent.duration._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import play.api.Play._
import play.api.mvc._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.pattern.ask
import akka.util._
import models.guide._
import models.plan._
import scalax.collection.edge.WUnDiEdge

object Preview extends Controller {
  private implicit val timeout: Timeout = 100.millis
  private implicit def system = Akka.system
  protected def guider = system.actorSelection(Akka.system / "guider")

  def show = Action.async {
    for {
      Guider.CurrentStep(step) <- guider ? Guider.QueryStep
      finalState = step.last.stateAfter
      output = finalState.output2
      graph = output.asGraph
    } yield {
      val alias = graph.nodes.zipWithIndex.map {
        case (node, index) => node.value -> s"n-$index"
      }.toMap
      val root = DotRootGraph(directed = false, id = None)
      def trans(e: Graph[Stitch2,WUnDiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
        Some((root, DotEdgeStmt(alias(e.edge._1.value), alias(e.edge._2.value), Nil)))
      }
      val dot = graph.toDot(root, trans _)
      Ok(views.html.preview(dot))
    }
  }

}