package models.planners

import scalaz._
import Scalaz._
import models._
import models.plan._

object Basics {
  /**
   *  Threads the yarn in A and leaves B empty if not already so.
   *  Returns the yarn configuration before this.
   */
  def oneYarn(yarn: Yarn) = yarns(Some(yarn))

  /**
   *  Threads the yarn in A and B if not already so.
   *  Returns the yarn configuration before this.
   */
  def twoYarns(y: (Yarn, Yarn)) = yarns(Some(y._1), Some(y._2))

  /**
   *  Threads the yarn in A and B if not already so.
   *  Returns the yarn configuration before this.
   */
  def yarns(yarnA: Option[Yarn], yarnB: Option[Yarn] = None) = {
    for {
      oldA <- Planner.state(_.yarnA)
      oldB <- Planner.state(_.yarnB)
      _ <- {
        if ((oldA, oldB) == (yarnA, yarnB)) Planner.noop
        else Planner.step(ThreadYarn(yarnA, yarnB))
      }
    } yield (oldA, oldB)
  }

  /** Knit a row. */
  def knitRow(carriage: CarriageType, pattern: Option[NeedleActionRow] = None) = for {
    dir <- Planner.validate(_.nextDirection(carriage))
    _ <- KnitRow(carriage, dir, pattern)
  } yield ()

  /** Change carriage settings (if necessary). Return the old settings. */
  def carriageSettings(settings: CarriageSettings) = for {
    old <- Planner.state(_.carriageSettings.get(settings.carriage))
    _ <- {
      if (old == Some(settings)) Planner.noop
      else Planner.step(ChangeCarriageSettings(settings))
    }
  } yield old
}