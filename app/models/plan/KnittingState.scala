package models.plan

import models._

case class KnittingState(needles: NeedlePatternRow,
  yarnA: Option[Yarn], yarnB: Option[Yarn],
  carriages: Map[CarriageType, (CarriageSettings, CarriagePosition)],
  output: Knitted) {
}

object KnittingState {
  val initial = KnittingState(_ => NeedleA, None, None, Map.empty, Knitted.empty)
}

