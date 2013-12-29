package models.plan

import models._

case class KnittingState(needles: NeedleStateRow,
  yarnA: Option[Yarn], yarnB: Option[Yarn],
  carriages: Map[CarriageType, (CarriageSettings, CarriagePosition)],
  output: Knitted) {

  def moveCarriage(carriage: CarriageType, pos: CarriagePosition) = {
    val (settings, _) = carriages.get(carriage).getOrElse(throw new IllegalStateException(s"undefined carriage: $carriage"))
    copy(carriages = carriages + (carriage -> (settings, pos)))
  }
  def moveNeedles(newNeedles: NeedleStateRow) = copy(needles = newNeedles)
  def knit(row: KnittedRow) = copy(output = output + row)
}

object KnittingState {
  val initial = KnittingState(_ => NeedleState(NeedleA), None, None, Map.empty, Knitted.empty)
}

