package knit.planners

import scalaz._
import Scalaz._
import knit._
import knit.plan._

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
    _ <- Basics.needCarriage(LCarriage, Left)
    positionOfL <- Planner.state(_.carriageState(LCarriage).position.directionTo(Needle.middle).towards)
    _ <- (depth - 1 to 0 by -1).toVector.traverse { offset =>
      val leftPattern: NeedleActionRow = if (left) (n => if (n == first - offset) NeedleToD else NeedleToB) else (n => NeedleToB)
      val rightPattern: NeedleActionRow = if (right) (n => if (n == last + offset) NeedleToD else NeedleToB) else (n => NeedleToB)

      if (positionOfL == Left) {
        Basics.knitRowWithL(LCarriage.Settings(LCarriage.Lace), leftPattern) >>
          Basics.knitRowWithL(LCarriage.Settings(LCarriage.Lace), rightPattern)
      } else {
        Basics.knitRowWithL(LCarriage.Settings(LCarriage.Lace), rightPattern) >>
          Basics.knitRowWithL(LCarriage.Settings(LCarriage.Lace), leftPattern)
      }
    }
    needlePosition <- Planner.state(_.needles(MainBed).positions)
    _ <- MoveNeedles(MainBed, n => if ((n == first && left) || (n == last && right)) NeedleA else needlePosition(n))
  } yield ()
}