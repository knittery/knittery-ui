package models.planners

import scalaz._
import Scalaz._
import models._
import models.plan._

/** Adds knitting marks to stitches. */
object Marker {

  def lastRow(as: KnittingMark): Planner = MarkRow(as)

}
