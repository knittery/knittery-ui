package models

sealed trait CarriageSettings {
  val carriage: Carriage
}

sealed trait HoldingCamLever {
  def name: String
}
case object HoldingCamN extends HoldingCamLever {
  override def name = "N"
}
case object HoldingCamH extends HoldingCamLever {
  override def name = "H"
}
//case object HoldingCamI extends HoldingCamLever  //TODO implement 

sealed trait KKnob
case object NL extends KKnob
case object CR extends KKnob
//do not use KCI, always use KCII, since we control the pattern. Therefore we don't implement it.
case object KC2 extends KKnob

// TODO complete
case class KCarriageSettings(knob: KKnob, mc: Boolean = false,
  holdingCamLever: HoldingCamLever = HoldingCamN) extends CarriageSettings {
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