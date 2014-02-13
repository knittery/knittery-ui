package models.planners

import scalaz._
import Scalaz._
import models._
import models.plan._

object FormGiving {

  /** Decrease by one using the 3-needle raglan method. */
  def raglanDecrease(bed: Bed, at: Needle, direction: Direction): Planner = {
    val sign = if (direction == ToLeft) -1 else 1
    (0 until (sign * 3) by sign).map(at + _).reverse.toVector.
      traverse { n =>
        Planner.step(RetireNeedle(bed, n, direction))
      } >> Planner.noop
  }

  /** Decrease one needle at the specified site (3-needle raglan). */
  def raglanDecrease(bed: Bed, at: LeftRight): PlannerM[Needle] = for {
    needle <- Planner.precondidtions { s =>
      val working = s.workingNeedles
      require(working.nonEmpty, "No working needles, cannot perform decrease")
      if (at == Left) working.head else working.last
    }
    direction = if (at == Left) ToRight else ToLeft
    _ <- raglanDecrease(bed, needle, direction)
  } yield needle
}