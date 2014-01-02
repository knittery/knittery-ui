package models.planner

import scalaz._
import Scalaz._
import models._
import models.plan._
import utils._

/**
 * Knits a fair isle pattern (aka Norwegermuster).
 */
object FairIslePlanner {
  /**
   *  Knit the pattern as a single bed fair isle pattern (row 0 in matrix is knitted first).
   *  Does not change the working position of the needles, at least one needle (or better all for the
   *  pattern) must be in working position.
   */
  def singleBed(pattern: Matrix[Yarn], startNeedle: Option[Needle] = None) = for {
    workingNeedles <- Planner.state(_.workingNeedles)
    //Check if valid pattern and state
    _ <- Planner.precondidtions { _ =>
      pattern.validate
      pattern.rows.map(_.toSet).zipWithIndex.foreach {
        case (yarns, index) =>
          require(yarns.size <= 2,
            s"fair isle pattern only support max two yarns per row. Row $index uses ${yarns.mkString(", ")}")
      }
      require(workingNeedles.nonEmpty, "No working needles")
    }
    needle0 = startNeedle.getOrElse(workingNeedles.head)
    _ <- setupCarriage
    _ <- pattern.rows.toVector.map { row =>
      setupYarn(row.toSet) >> knitPatternRow(needle0)(row)
    }.sequence
  } yield ()

  /** Knits a row in the pattern. Requires that the yarn is setup properly. */
  private def knitPatternRow(startNeedle: Needle)(row: IndexedSeq[Yarn]) = for {
    yarnA <- Planner.state(_.yarnA)
    yarnB <- Planner.state(_.yarnB)
    knitActions = (needle: Needle) => {
      val index = needle.index - startNeedle.index
      if (index < 0 || index > row.size) NeedleToB
      else Some(row(index)) match {
        case `yarnA` => NeedleToB
        case `yarnB` => NeedleToD
        case Some(x) => throw new IllegalStateException(s"want to use yarn $x but that's not on the carriage")
      }
    }
    dir <- Planner.validate(_.nextDirection(KCarriage))
    _ <- KnitPatternRow(KCarriage, dir, knitActions)
  } yield ()

  /** Make sure the yarns are available in the carriage, issue a step if not. */
  private def setupYarn(required: Set[Yarn]) = for {
    available <- Planner.state(s => s.yarnA.toSet ++ s.yarnB.toSet)
    (yarnA, yarnB) = required.toList match {
      case a :: b :: Nil => (Some(a), Some(b))
      case a :: Nil => (Some(a), None)
      case other => throw new IllegalStateException(s"Invalid yarn configuration: $other")
    }
    _ <- {
      if (!required.forall(available.contains)) Planner.step(ThreadYarn(yarnA, yarnB)) //change yarn
      else Planner.noop //yarn is ok
    }
  } yield ()

  /** Carriage settings and make sure carriage is known. */
  private def setupCarriage = for {
    _ <- ChangeKCarriageSettings(KCarriageSettings(
      holdingCamLever = HoldingCamN,
      knob = KC2,
      mc = true))
    kCarriageDefined <- Planner.state(_.carriagePosition.isDefinedAt(KCarriage))
    _ <- if (!kCarriageDefined) Planner.step(AddCarriage(KCarriage)) else Planner.noop
  } yield ()
}
