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

  /** Knit a pattern row. */
  def knitPatternRow(carriage: CarriageType, pattern: NeedleActionRow) = for {
    dir <- Planner.validate(_.nextDirection(carriage))
    _ <- KnitPatternRow(carriage, dir, pattern)
  } yield ()

  /** Change carriage settings (if necessary). Return the old settings. */
  def carriageSettings(settings: CarriageSettings) = for {
    old <- Planner.state(_.carriageSettings.get(settings.carriage))
    _ <- {
      if (old == Some(settings)) Planner.noop
      else settings match {
        case settings: KCarriageSettings => Planner.step(ChangeKCarriageSettings(settings))
        case settings: LCarriageSettings => Planner.step(ChangeLCarriageSettings(settings))
        case settings: GCarriageSettings => Planner.step(ChangeGCarriageSettings(settings))
      }
    }
  } yield old
}