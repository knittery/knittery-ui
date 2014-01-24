package models.planners

import scalaz._
import Scalaz._
import models._
import models.plan._

object Cast {
  def onClosed(from: Needle, until: Needle, withYarn: Yarn): PlannerM[YarnFlow] = {
    Basics.yarn(YarnStart(withYarn)) >>=
      (y => onClosed(from, until, y))
  }
  def onClosed(from: Needle, until: Needle, withYarn: YarnFlow): PlannerM[YarnFlow] = {
    Planner.step(ClosedCastOn(from, until, withYarn)) >>
      Basics.yarnAttachment(withYarn).map(_.get.yarn)
  }

  def offClosed(withYarn: YarnFlow, filter: Needle => Boolean = _ => true): Planner = for {
    needleState <- Planner.state(_.needles)
    _ <- ClosedCastOff(withYarn, n => filter(n) && needleState(n).yarn.nonEmpty)
  } yield ()
}