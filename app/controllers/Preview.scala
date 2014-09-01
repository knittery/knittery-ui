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

  private def alias(stitch: Stitch3D, in: Knitted3D) = {
    val index = in.stitches.indexOf(stitch)
    assert(index != -1, "Alias for non-existing stitch")
    index.toString
  }

  def json = GuiderAction { req =>
    val knitted3D = req.knitted3d
    val graph = req.knitted3d.asGraph
    val layout = LayoutOps(req.layout, knitted3D.stitches).inside(Box3(3000))

    val nodeJson = knitted3D.stitches.map { node =>
      val yarns = node.points.map(_.yarn).toSet
      Json.obj(
        "id" -> alias(node, knitted3D),
        "colors" -> yarns.map(_.color).map(colorRgb),
        "position" -> layout(node))
    }
    val edgeJson = graph.edges.map { edge =>
      val color = edge.label match {
        case Yarn(_, color) => color
      }
      Json.obj(
        "n1" -> alias(edge._1, knitted3D),
        "n2" -> alias(edge._2, knitted3D),
        "weight" -> edge.weight,
        "color" -> colorRgb(color))
    }
    val json = Json.obj(
      "nodes" -> nodeJson,
      "edges" -> edgeJson)
    Ok(json)
  }

  def dot = GuiderAction { req =>
    val dotRoot = DotRootGraph(directed = false, id = None)
    val dot = req.knitted3d.asGraph.toDot(dotRoot, e =>
      Some((dotRoot, DotEdgeStmt(
        "s-" + alias(e.edge._1.value, req.knitted3d),
        "s-" + alias(e.edge._2.value, req.knitted3d), Nil)))
    )
    Ok(dot).as("text/vnd.graphviz")
  }

  class RequestWithLayout[A](val knitted3d: Knitted3D, val layout: Layout[Stitch3D], request: Request[A])
    extends WrappedRequest[A](request)
  case object GuiderAction extends ActionBuilder[RequestWithLayout] {
    override def invokeBlock[A](request: Request[A], block: (RequestWithLayout[A]) ⇒ Future[Result]) = {
      for {
        Guider.Knitted3DLayout(knitted, layout) <- guider ? Guider.GetKnitted3D
        req = new RequestWithLayout(knitted, layout, request)
        result <- block(req)
      } yield result
    }
  }
}