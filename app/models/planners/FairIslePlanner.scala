package models.planners

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
  def singleBed(pattern: Matrix[Yarn], backgroundYarn: Yarn, startNeedle: Option[Needle] = None) = for {
    workingNeedles <- Planner.state(_.workingNeedles)
    //Check if valid pattern and state
    _ <- Planner.precondidtions { _ =>
      pattern.validate
      require(pattern.height > 0, "Empty pattern")
      pattern.rows.map(_.toSet).zipWithIndex.foreach {
        case (yarns, index) =>
          require(yarns.size <= 2,
            s"fair isle pattern only support max two yarns per row. Row $index uses ${yarns.mkString(", ")}")
      }
      require(workingNeedles.nonEmpty, "No working needles")
    }
    needle0 = startNeedle.getOrElse(workingNeedles.head)
    settings = KCarriageSettings(holdingCamLever = HoldingCamN, knob = KC2, mc = true)
    _ <- Basics.needCarriage(KCarriage)
    //Knit the pattern rows
    _ <- pattern.rows.toVector.traverse(row => for {
      yarns <- optimizeYarn(row.toSet)
      actionRow = knitActions(row, needle0, yarns) _
      _ <- Basics.knitPatternRow(settings, actionRow, yarns._1, yarns._2)
    } yield ())
  } yield ()

  //TODO also take into account the next rows to knit => make it a general optimization?
  private def optimizeYarn(required: Set[Yarn]) = {
    for {
      yarnA <- Planner.state(_.yarnA)
      yarnB <- Planner.state(_.yarnB)
      available <- Planner.state(_.yarns)
    } yield {
      if (required.forall(available.contains)) (yarnA, yarnB)
      else required.toList match {
        case one :: two :: Nil => (Some(one), Some(two))
        case one :: Nil => (Some(one), None)
        case other => throw new IllegalStateException(s"Invalid yarn configuration: $other")
      }
    }
  }

  def knitActions(row: Seq[Yarn], startNeedle: Needle, yarns: (Option[Yarn], Option[Yarn]))(needle: Needle) = {
    val index = needle.index - startNeedle.index
    if (index < 0 || index > row.size) NeedleToB
    else Some(row(index)) match {
      case yarns._1 => NeedleToB
      case yarns._2 => NeedleToD
      case Some(x) => throw new IllegalStateException(s"want to use yarn $x but that's not on the carriage")
    }
  }
}
