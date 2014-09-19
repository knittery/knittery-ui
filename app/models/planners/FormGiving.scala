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


  /** Perform an automated raglan decrease on the main bed with the L carriage. */
  def raglanWithLCarriage(depth: Int, left: Boolean = true, right: Boolean = true): Planner = for {
    _ <- Planner.precondidtions(s => require(s.workingNeedles(MainBed).size >= depth * 2, "Not enough working needles"))
    first <- Planner.state(_.workingNeedles(MainBed).head)
    last <- Planner.state(_.workingNeedles(MainBed).last)
    positionOfL <- Planner.state(_.carriageState(KCarriage).position.directionTo(Needle.middle).towards)
    _ <- Basics.needCarriage(LCarriage, positionOfL)
    _ <- (depth - 1 to 0 by -1).toVector.traverse { offset =>
      val ld =
        if (left) Some(Basics.knitRowWithL(LCarriage.Settings(LCarriage.Lace), n => if (n == first + offset) NeedleToD else NeedleToB))
        else None
      val rd =
        if (right) Some(Basics.knitRowWithL(LCarriage.Settings(LCarriage.Lace), n => if (n == last - offset) NeedleToD else NeedleToB))
        else None
      val actions =
        if (positionOfL == Left) ld.toVector ++ rd.toVector
        else rd.toVector ++ ld.toVector
      actions.sequence
    }
    needlePosition <- Planner.state(_.needles(MainBed).positions)
    _ <- MoveNeedles(MainBed, n => if (n == first || n == last) NeedleA else needlePosition(n))
  } yield ()
}