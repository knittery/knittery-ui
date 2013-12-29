package models.plan

import scala.util.Try
import scalaz._
import Scalaz._
import models._
import utils._

/** Step to perform during knitting. */
sealed trait KnittingStep extends (KnittingState => Validation[String, KnittingState])

/** Knits a row using a carriage. */
sealed trait KnitARow extends KnittingStep {
  def carriage: CarriageType
  def direction: Direction
  protected def needleActionRow: Option[NeedleActionRow]

  override def apply(state: KnittingState) = {
    for {
      kc <- knittingCarriage(state, needleActionRow)
      (needles, knitted) <- kc(direction)(state.needles)
    } yield {
      state.
        moveCarriage(carriage, direction).
        moveNeedles(needles).
        knit(knitted)
    }
  }
  protected def knittingCarriage(state: KnittingState, pattern: Option[NeedleActionRow]) = {
    for {
      pos <- state.carriagePosition.get(carriage).toSuccess(s"Undefined position for $carriage")
      settings <- state.carriageSettings.get(carriage).toSuccess(s"Undefined settings for $carriage")
      c = KnittingCarriage(carriage, settings, state.yarnA, state.yarnB, pattern)
      nextDir <- state.nextDirection(carriage)
      _ <- {
        if (nextDir != direction) s"Cannot move carriage from $direction to $direction".fail[KnittingCarriage]
        else ().success
      }
    } yield c
  }
}

case class KnitRow(carriage: CarriageType, direction: Direction, yarnA: Option[Yarn]) extends KnitARow {
  override protected def needleActionRow = None
  def yarnB = None
}

case class KnitPatternRow(carriage: CarriageType, direction: Direction, pattern: NeedleActionRow) extends KnitARow {
  override protected def needleActionRow = Some(pattern)
}

sealed trait ChangeCarriageSettings extends KnittingStep {
  val settings: CarriageSettings
  def carriage: CarriageType = settings.carriage
  override def apply(state: KnittingState) = {
    state.modifyCarriageSettings(settings).success[String]
  }
}
case class ChangeKCarriageSettings(settings: KCarriageSettings) extends ChangeCarriageSettings
case class ChangeLCarriageSettings(settings: LCarriageSettings) extends ChangeCarriageSettings
case class ChangeGCarriageSettings(settings: GCarriageSettings) extends ChangeCarriageSettings

trait ManualAction extends KnittingStep {
  def name: String

  /** Description of the action to perform. May use basic HTML syntax. */
  def description: String
}
