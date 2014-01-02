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

case class KnitRow(carriage: CarriageType, direction: Direction) extends KnitARow {
  override protected def needleActionRow = None
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

case class ThreadYarn(yarnA: Option[Yarn], yarnB: Option[Yarn]) extends KnittingStep {
  override def apply(state: KnittingState) = {
    state.copy(yarnA = yarnA, yarnB = yarnB).success[String]
  }
}

/**
 *  Performs a closed cast on for the needles. The needles are then moved to D position.
 *  All other needles are not touched.
 */
case class ClosedCastOn(from: Needle, until: Needle, yarn: Yarn) extends KnittingStep {
  def needles = Needle.interval(from, until)
  override def apply(state: KnittingState) = {
    state.
      moveNeedles { n =>
        val before = state.needles(n)
        if (needles.contains(n)) {
          NeedleState(NeedleD, yarn :: before.yarn)
        } else before
      }.
      knit { n =>
        if (needles.contains(n)) CastOnStich(yarn)
        else NoStich
      }.
      success[String]
  }
}

case class AddCarriage(carriage: CarriageType, from: Direction = Left) extends KnittingStep {
  override def apply(state: KnittingState) =
    state.moveCarriage(carriage, from.reverse).success
}

trait ManualAction extends KnittingStep {
  def name: String

  /** Description of the action to perform. May use basic HTML syntax. */
  def description: String
}
