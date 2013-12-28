package models

sealed trait CarriageSettings {
  val carriage: CarriageType
}

sealed trait HoldingCamLever
case object HoldingCamN extends HoldingCamLever
case object HoldingCamH extends HoldingCamLever
//case object HoldingCamI extends HoldingCamLever  //TODO implement 

sealed trait KKnob
case object KKnobNL extends KKnob
case object KKnobCR extends KKnob
//do not use KCI, always use KCII, since we control the pattern. Therefore we don't implement it.
case object KKnobKC2 extends KKnob

// TODO complete
case class KCarriageSettings(holdingCamLever: HoldingCamLever, knob: KKnob) extends CarriageSettings {
  val carriage = KCarriage
}

// TODO complete
case class LCarriageSettings() extends CarriageSettings {
  val carriage = LCarriage
}

// TODO complete
case class GCarriageSettings() extends CarriageSettings {
  val carriage = GCarriage
}