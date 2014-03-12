package models.guide

import scala.concurrent.duration._
import akka.actor.{ActorLogging, Props, Actor}
import models.plan.{Knitted3D, Stitch3D}
import ch.inventsoft.graph.layout._
import ch.inventsoft.graph.layout.spring.BarnesHutLayout

private object Layouter {
  sealed trait Command
  sealed trait Event

  /** Get the current layout. Will be answered with a Layout[Stitch3D] */
  case object Get extends Command
  /** Abort the layouting. Will be answered with a Layout[Stitch3D] */
  case object Abort extends Command

  /** Will be sent to the parent when layouting is done. */
  case class Done(layout: Layout[Stitch3D]) extends Event

  private case object Improve extends Command

  def props(knitted: Knitted3D) = {
    val layout = BarnesHutLayout(knitted.asGraph, knitted.asLayout, 1d)
    Props(new Layouter(layout, 2000, knitted.asGraph.nodes.size))
  }


  private class Layouter(initial: IncrementalLayout[Stitch3D], limit: Int, size: Int) extends Actor with ActorLogging {
    var stepNr = 0
    var layout: IncrementalLayout[Stitch3D] = initial

    private val startTime = System.currentTimeMillis
    def since = (System.currentTimeMillis - startTime).millis

    override def preStart() = {
      log.debug(s"Will layout $size nodes..")
      self ! Improve
    }

    override def receive = {
      case Improve =>
        layout = layout.improve
        stepNr = stepNr + 1
        if (stepNr < limit) {
          self ! Improve
        } else {
          log.info(s"Done layouting $size nodes after $stepNr steps in $since")
          context.parent ! Done(layout)
        }

      case Get =>
        sender ! layout
      case Abort =>
        sender ! layout
        context stop self
    }
  }
}
