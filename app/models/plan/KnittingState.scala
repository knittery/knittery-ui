package models.plan

import models._

case class KnittingState(needles: NeedleStateRow,
  yarnA: Option[Yarn], yarnB: Option[Yarn],
  carriageSettings: Map[CarriageType, CarriageSettings],
  carriagePosition: Map[CarriageType, CarriagePosition],
  output: Knitted) {

  def modifyCarriageSettings(settings: CarriageSettings) =
    copy(carriageSettings = carriageSettings + (settings.carriage -> settings))
  def moveCarriage(carriage: CarriageType, pos: CarriagePosition) =
    copy(carriagePosition = carriagePosition + (carriage -> pos))
  def moveCarriage(carriage: CarriageType, direction: Direction): KnittingState =
    moveCarriage(carriage, if (direction == Left) CarriageLeft(0) else CarriageRight(0))
  def moveNeedles(newNeedles: NeedleStateRow) = copy(needles = newNeedles)
  def knit(row: KnittedRow) = copy(output = output + row)
}

object KnittingState {
  val initial = KnittingState(_ => NeedleState(NeedleA), None, None, Map.empty, Map.empty, Knitted.empty)
}

