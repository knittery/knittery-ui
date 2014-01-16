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

  /** Add carriage if missing. */
  def needCarriage(carriage: CarriageType, at: LeftRight = Left) = for {
    p <- Planner.state(_.carriagePosition.get(carriage))
    _ <- if (p.isEmpty) Planner.step(AddCarriage(carriage, at)) else Planner.noop
  } yield ()

  /** Change carriage settings. */
  def carriageSettings(settings: CarriageSettings) = for {
    _ <- needCarriage(settings.carriage)
    current <- Planner.state(_.carriageSettings.get(settings.carriage))
    _ <- if (current == Some(settings)) Planner.noop else Planner.step(ChangeCarriageSettings(settings))
  } yield current

  /** Next direction for the carriage. */
  def nextDirection(carriage: CarriageType) = Planner.validate(_.nextDirection(carriage))

  /** Knit a row. */
  def knitRow(settings: CarriageSettings, yarn: Option[Yarn]) = for {
    _ <- carriageSettings(settings)
    _ <- yarns(yarn)
    dir <- Planner.validate(_.nextDirection(settings.carriage))
    _ <- KnitRow(settings.carriage, dir)
  } yield ()

  /** Knit a row with a pattern. */
  def knitPatternRow(settings: CarriageSettings, pattern: NeedleActionRow, yarnA: Option[Yarn], yarnB: Option[Yarn]) = for {
    _ <- carriageSettings(settings)
    before <- Planner.state(_.needles.positions)
    _ <- MoveNeedles(before, pattern)
    _ <- yarns(yarnA, yarnB)
    dir <- nextDirection(settings.carriage)
    _ <- KnitRow(settings.carriage, dir)
  } yield ()
}