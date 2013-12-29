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
  def singleBed(pattern: Matrix[Yarn], startNeedle: Option[Needle] = None) = {
    val validate = Planner.validateTry { state =>
      pattern.validate
      pattern.rows.map(_.toSet).zipWithIndex.foreach {
        case (yarns, index) =>
          require(yarns.size <= 2,
            s"fair isle pattern only support max two yarns per row. Row $index uses ${yarns.mkString(", ")}")
      }
      require(state.workingNeedles.nonEmpty, "No working needles")
    }
    def knitting(start: Needle) = pattern.rows.map { row =>
      setupYarn(row.toSet) + knitPatternRow(row, start)
    }.reduce(_ + _)

    validate.append { initial =>
      val start = startNeedle.getOrElse(initial.workingNeedles.head)
      setupCarriage + knitting(start)
    }
  }

  private def knitPatternRow(row: IndexedSeq[Yarn], startNeedle: Needle) = {
    Planner.step { state =>
      def actions(needle: Needle) = {
        val index = needle.index - startNeedle.index
        if (index < 0 || index > row.size) NeedleToB
        else Some(row(index)) match {
          case state.yarnA => NeedleToB
          case state.yarnB => NeedleToD
          case Some(x) => throw new IllegalStateException(s"want to use yarn $x but that's not on the carriage")
        }
      }
      state.nextDirection(KCarriage).map { dir =>
        KnitPatternRow(KCarriage, dir, actions)
      }
    }
  }

  /** Make sure the yarns are available in the carriage, issue a step if not. */
  private def setupYarn(required: Set[Yarn]) = {
    Planner.optStep { state =>
      val available = state.yarnA.toSet ++ state.yarnB.toSet
      if (!required.forall(available.contains)) {
        // we need to change the yarn
        val (a, b) = required.toList match {
          case a :: b :: Nil => (Some(a), Some(b))
          case a :: Nil => (Some(a), None)
          case other => throw new IllegalStateException(s"Invalid yarn configuration: $other")
        }
        //Change yarn and come again
        Some(ThreadYarn(a, b)).success
      } else None.success
    }
  }

  /** Carriage settings and make sure carriage is known. */
  private def setupCarriage = {
    val settings = Planner.step { state =>
      ChangeKCarriageSettings(KCarriageSettings(
        holdingCamLever = HoldingCamN,
        knob = KC2,
        mc = true)).success
    }
    val addCarriage = Planner.optStep { state =>
      {
        if (state.carriagePosition.isDefinedAt(KCarriage)) None
        else Some(AddCarriage(KCarriage))
      }.success
    }
    addCarriage + settings
  }
}
