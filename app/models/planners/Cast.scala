package models.planners

import scalaz._
import Scalaz._
import models._
import models.plan._

object Cast {
  def onClosed(bed: Bed, from: Needle, until: Needle, withYarn: Yarn): PlannerM[YarnPiece] =
    onClosed(bed, from, until, YarnPiece(withYarn))
  def onClosed(bed: Bed, from: Needle, until: Needle, withYarn: YarnPiece): PlannerM[YarnPiece] = {
    Planner.step(ClosedCastOn(bed, from, until, withYarn)).map(_ => withYarn)
  }

  def onClosedRound(from: Needle, until: Needle, yarn: YarnPiece) = for {
    _ <- Planner.noop
    until2 <- Planner.precondidtions { _ =>
      require(from <= until, s"Cannot perform closed round cast on from right to left ($from -> $until)")
      val u2 = until.index + (until distanceTo from) - 1
      require(u2 < Needle.count, "Needle bed not wide enough")
      Needle.atIndex(u2)
    }
    //TODO basically we'd need to knit with contrast yarn first in order to
    // be able to move the yarn properly..
    _ <- onClosed(MainBed, from, until2, yarn)
    _ <- Basics.needCarriage(KCarriage, Right)
    _ <- Basics.knitRowWithK(yarnA = Some(yarn))
    _ <- MoveToDoubleBed(n => n >= until && n <= until2, -1, Some(until))
    _ <- MoveNeedles(MainBed, (n: Needle) => if (n >= from && n < until) NeedleB else NeedleA)
  } yield yarn

  def offClosed(bed: Bed, withYarn: YarnPiece, filter: Needle => Boolean = _ => true): Planner = for {
    needleState <- Planner.state(_.needles(bed))
    _ <- ClosedCastOff(bed, withYarn, n => filter(n) && needleState(n).yarn.nonEmpty)
  } yield ()
}