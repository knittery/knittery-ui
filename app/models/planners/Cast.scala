package models.planners

import scalaz._
import Scalaz._
import models._
import models.plan._

object Cast {
  def onClosed(from: Needle, until: Needle, withYarn: Yarn): PlannerM[YarnFlow] =
    onClosed(from, until, YarnPiece(withYarn))
  def onClosed(from: Needle, until: Needle, withYarn: YarnPiece): PlannerM[YarnFlow] = {
    Planner.step(ClosedCastOn(from, until, withYarn)) >>
      Basics.yarnAttachment(withYarn).map(_.get.yarn)
  }

  def offClosed(withYarn: YarnPiece, filter: Needle => Boolean = _ => true): Planner = for {
    needleState <- Planner.state(_.needles)
    _ <- ClosedCastOff(withYarn, n => filter(n) && needleState(n).yarn.nonEmpty)
  } yield ()
}