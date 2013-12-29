package models.plan

import scala.util.Try
import models._

/** Step to perform during knitting. */
sealed trait KnittingStep extends Function[KnittingState, Try[KnittingState]] {
  /** Check if the step can be applied to the state. */
  def checkPreconditions(state: KnittingState) = apply(state).isSuccess

  protected def invalidState(reason: String) = throw new IllegalStateException(reason)
}

/** Knits a row using a carriage. */
sealed trait KnitARow extends KnittingStep {
  def carriage: CarriageType
  def direction: Direction
  protected def needleActionRow: Option[NeedleActionRow]

  def yarnA: Option[Yarn]
  def yarnB: Option[Yarn]

  override def apply(state: KnittingState) = {
    val c = knittingCarriage(state, needleActionRow)
    c.flatMap(_(direction)(state.needles)).map {
      case (needles, knitted) => state.
        moveCarriage(carriage, if (direction == Left) CarriageLeft(0) else CarriageRight(0)).
        moveNeedles(needles).
        knit(knitted)
    }
  }
  protected def knittingCarriage(state: KnittingState, pattern: Option[NeedleActionRow]) = Try {
    val pos = state.carriagePosition.get(carriage).getOrElse(invalidState(s"Undefined position for $carriage"))
    val settings = state.carriageSettings.get(carriage).getOrElse(invalidState(s"Undefined settings for $carriage"))
    val c = KnittingCarriage(carriage, settings, yarnA, yarnB, pattern)
    //check preconditions
    (pos, direction) match {
      case (CarriageLeft(_), Left) => invalidState("Cannot move carriage from left to left")
      case (CarriageRight(_), Right) => invalidState("Cannot move carriage from right to right")
      case (_, _) => () // ok
    }
    c
  }
}

case class KnitRow(carriage: CarriageType, direction: Direction, yarnA: Option[Yarn]) extends KnitARow {
  override protected def needleActionRow = None
  def yarnB = None
}

case class KnitPatternRow(carriage: CarriageType, direction: Direction, pattern: NeedleActionRow,
  yarnA: Option[Yarn], yarnB: Option[Yarn] = None) extends KnitARow {
  override protected def needleActionRow = Some(pattern)
}

sealed trait ChangeCarriageSettings extends KnittingStep {
  val settings: CarriageSettings
  def carriage: CarriageType = settings.carriage
  override def apply(state: KnittingState) = Try {
    state.modifyCarriageSettings(settings)
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
