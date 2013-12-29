package models.plan

import models._

case class KnittingState(needles: NeedleStateRow,
  yarnA: Option[Yarn], yarnB: Option[Yarn],
  carriageSettings: Map[CarriageType, CarriageSettings],
  carriagePosition: Map[CarriageType, CarriagePosition],
  output: Knitted) {

  def moveCarriage(carriage: CarriageType, pos: CarriagePosition) =
    copy(carriagePosition = carriagePosition + (carriage -> pos))
  def moveNeedles(newNeedles: NeedleStateRow) = copy(needles = newNeedles)
  def knit(row: KnittedRow) = copy(output = output + row)
}

object KnittingState {
  val initial = KnittingState(_ => NeedleState(NeedleA), None, None, Map.empty, Map.empty, Knitted.empty)
}

