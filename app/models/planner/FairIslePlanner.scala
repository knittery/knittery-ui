package models.planner

import scalaz._
import Scalaz._
import models._
import models.plan._
import utils._
import PlannerUtils._

/**
 * Knits a fair isle pattern (aka Norwegermuster).
 */
object FairIslePlanner {
  /**
   *  Knit the pattern as a single bed fair isle pattern (row 0 in matrix is knitted first).
   *  Does not change the working position of the needles, at least one needle (or better all for the
   *  pattern) must be in working position.
   */
  def singleBed(pattern: Matrix[Yarn], startNeedle: Option[Needle] = None) = Planner { initial =>
    pattern.validate
    pattern.rows.map(_.toSet).zipWithIndex.foreach {
      case (yarns, index) =>
        require(yarns.size <= 2,
          s"fair isle pattern only support max two yarns per row. Row $index uses ${yarns.mkString(", ")}")
    }
    require(initial.workingNeedles.nonEmpty, "No working needles")

    val start = startNeedle.getOrElse(initial.workingNeedles.head)
    def actions(row: Seq[Yarn], yarnA: Option[Yarn], yarnB: Option[Yarn])(needle: Needle): NeedleAction = {
      val index = needle.index - start.index
      if (index < 0 || index > pattern.width) NeedleToB
      else Some(row(index)) match {
        case `yarnA` => NeedleToB
        case `yarnB` => NeedleToD
        case Some(x) => throw new IllegalStateException(s"want to use yarn $x but that's not on the carriage")
      }
    }

    val step0 = ChangeKCarriageSettings(KCarriageSettings(
      holdingCamLever = HoldingCamN,
      knob = KC2,
      mc = true))

    pattern.rows.foldStep(StepAcc(step0, initial)) { (state, row) =>
      //See if we have the right yarns available
      val availableYarns = state.yarnA.toSet ++ state.yarnB.toSet
      val neededYarns = row.toSet
      if (!neededYarns.forall(availableYarns.contains)) {
        // we need to change the yarn
        val (a, b) = neededYarns.toList match {
          case a :: b :: Nil => (Some(a), Some(b))
          case a :: Nil => (Some(a), None)
          case other => throw new IllegalStateException(s"Invalid yarn configuration: $other")
        }
        //Change yarn and come again
        (false, ThreadYarn(a, b)).success
      } else {
        //Yarn is correct, let's knit the row
        val p = actions(row, state.yarnA, state.yarnB) _
        for {
          dir <- state.nextDirection(KCarriage)
          step = KnitPatternRow(KCarriage, dir, p)
        } yield (true, step)
      }
    }.toPlan
  }
}
