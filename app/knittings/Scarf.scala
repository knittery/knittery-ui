package knittings

import java.awt.image.BufferedImage

import scalaz._
import Scalaz._
import utils.Matrix
import models._
import models.units._
import models.plan._
import models.planners.{FairIslePlanner, Cast, Helper}
import squants.space.Length

object Scarf {
  case class Dimensions(width: Stitches, height: Rows)
  type Pattern = Matrix[Yarn]

  def rectangular(length: Length, width: Length, pattern: Dimensions => Pattern)(implicit gauge: Gauge) = for {
    _ <- Planner.precondidtions(_ => require(width <= length, "width greater than length"))
    (first, last) = Helper.center(width.toStitches)
    p = pattern(Dimensions(width.toStitches, length.toRows))

    startColor = mostFrequent(p.head)
    _ <- Cast.onDoubleBed(first, last, YarnPiece(startColor))

    _ <- FairIslePlanner.doubleBed(p, gauge.tension, Some(first))

    endColor = mostFrequent(p.last)
    endPiece <- Planner.state(_.yarnAttachments.keys.find(_.yarn == endColor).getOrElse(YarnPiece(endColor)))
    _ <- Cast.offDoubleBed(YarnPiece(endColor))
  } yield ()

  private def mostFrequent[A](in: Traversable[A]): A = {
    require(in.nonEmpty, "mostFrequent in empty")
    in.groupBy(y => y).toSeq.sortBy(_._2.size).last._1
  }
}
