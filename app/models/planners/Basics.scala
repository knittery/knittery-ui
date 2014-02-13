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

  /** Change K-carriage assembly. */
  def assembly(a: KCarriage.Assembly) = for {
    _ <- needCarriage(KCarriage)
    current <- Planner.state(_.carriageState(KCarriage).assembly)
    _ <- {
      if (current == a) Planner.noop
      else Planner.step(ChangeKCarriageAssembly(a))
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

  def yarnAttachment(yarn: YarnFlow) = Planner.state(_.yarnAttachments.get(yarn.start))
  def yarnAttachment(yarn: Option[YarnFlow]) =
    Planner.state(s => yarn.flatMap(y => s.yarnAttachments.get(y.start)))
  def nearestYarn(yarn: Yarn) = {
    Planner.state(_.yarnAttachments).map { yas =>
      yas.filterKeys(_.yarn == yarn).toSeq.sortBy(_._2.rowDistance).
        map(_._2.yarn).headOption
    }
  }

  /** Knit a row with the K-Carriage. */
  def knitRowWithK(settings: KCarriage.Settings, yarnA: Option[YarnPiece] = None, yarnB: Option[YarnPiece] = None, pattern: NeedleActionRow = AllNeedlesToB) = for {
    _ <- carriageSettings(settings)
    needlesBefore <- Planner.state(_.needles.positions)
    _ <- MoveNeedles(needlesBefore, pattern)
    _ <- ThreadYarnK(yarnA, yarnB)
    dir <- nextDirection(KCarriage)
    _ <- KnitRow(KCarriage, dir)
  } yield ()

  /** Round knitting with the K-Carriage. */
  def knitRoundK(yarn: YarnPiece) = {
    import KCarriage._
    val assembly = DoubleBedCarriage(partRight = true, yarn = Some(yarn))
    val settings = Settings(partLeft = true)
    Basics.assembly(assembly) >>
      knitRowWithK(settings, Some(yarn))
  }

  /** Knit a row with the L-Carriage. */
  def knitRowWithL(settings: LCarriage.Settings, pattern: NeedleActionRow = AllNeedlesToB) = for {
    _ <- carriageSettings(settings)
    needlesBefore <- Planner.state(_.needles.positions)
    _ <- MoveNeedles(needlesBefore, pattern)
    dir <- nextDirection(LCarriage)
    _ <- KnitRow(LCarriage, dir)
  } yield ()

  /** Knit a row with the G-Carriage. */
  def knitRowWithG(settings: GCarriage.Settings, yarn: Option[YarnPiece] = None, pattern: NeedleActionRow = AllNeedlesToB) = for {
    _ <- carriageSettings(settings)
    needlesBefore <- Planner.state(_.needles.positions)
    _ <- ThreadYarnG(yarn)
    dir <- nextDirection(GCarriage)
    _ <- KnitRow(LCarriage, dir, pattern)
  } yield ()
}