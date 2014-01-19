package models.planners

import scalaz._
import Scalaz._
import models._
import models.plan._

object Basics {
  /** Add carriage if missing. */
  def needCarriage(carriage: Carriage, at: LeftRight = Left) = for {
    position <- Planner.state(_.carriageState(carriage).position)
    _ <- {
      if (position == CarriageRemoved) Planner.step(AddCarriage(KCarriage, at))
      else Planner.noop
    }
  } yield ()

  /** Change K-carriage settings. */
  def carriageSettings(settings: KCarriage.Settings) = for {
    _ <- needCarriage(KCarriage)
    current <- Planner.state(_.carriageState(KCarriage).settings)
    _ <- {
      if (current == settings) Planner.noop
      else Planner.step(ChangeKCarriageSettings(settings))
    }
  } yield current

  /** Change L-carriage settings. */
  def carriageSettings(settings: LCarriage.Settings) = for {
    _ <- needCarriage(KCarriage)
    current <- Planner.state(_.carriageState(LCarriage).settings)
    _ <- {
      if (current == settings) Planner.noop
      else Planner.step(ChangeLCarriageSettings(settings))
    }
  } yield current

  /** Change G-carriage settings. */
  def carriageSettings(settings: GCarriage.Settings) = for {
    _ <- needCarriage(KCarriage)
    current <- Planner.state(_.carriageState(GCarriage).settings)
    _ <- {
      if (current == settings) Planner.noop
      else Planner.step(ChangeGCarriageSettings(settings))
    }
  } yield current

  /** Next direction for the carriage. */
  def nextDirection(carriage: Carriage) = Planner.validate(_.nextDirection(carriage))

  /** Knit a row with the K-Carriage. */
  def knitRowWithK(settings: KCarriage.Settings, yarnA: Option[Yarn] = None, yarnB: Option[Yarn] = None, pattern: NeedleActionRow = AllNeedlesToB) = for {
    _ <- carriageSettings(settings)
    needlesBefore <- Planner.state(_.needles.positions)
    _ <- MoveNeedles(needlesBefore, pattern)
    _ <- ThreadYarnK(yarnA, yarnB)
    dir <- nextDirection(KCarriage)
    _ <- KnitRow(KCarriage, dir)
  } yield ()

  /** Knit a row with the L-Carriage. */
  def knitRowWithL(settings: LCarriage.Settings, pattern: NeedleActionRow = AllNeedlesToB) = for {
    _ <- carriageSettings(settings)
    needlesBefore <- Planner.state(_.needles.positions)
    _ <- MoveNeedles(needlesBefore, pattern)
    dir <- nextDirection(LCarriage)
    _ <- KnitRow(LCarriage, dir)
  } yield ()

  /** Knit a row with the G-Carriage. */
  def knitRowWithG(settings: GCarriage.Settings, yarnA: Option[Yarn] = None, pattern: NeedleActionRow = AllNeedlesToB) = for {
    _ <- carriageSettings(settings)
    needlesBefore <- Planner.state(_.needles.positions)
    _ <- ThreadYarnG(yarnA)
    dir <- nextDirection(GCarriage)
    _ <- KnitRow(LCarriage, dir, pattern)
  } yield ()
}