package models

sealed trait CarriageSettings {
  def carriage: CarriageType

  def applyOnNeedles(direction: Direction)(before: NeedlePatternRow): NeedlePatternRow
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
  def carriage = KCarriage

  def applyOnNeedles(dir: Direction)(before: NeedlePatternRow) = {
    if (knob == KKnobCR) throw new IllegalStateException("Not knitting in CR setting")
    before andThen {
      case NeedleE if holdingCamLever == HoldingCamH => NeedleE
      case NeedleE => NeedleB
      case NeedleD => NeedleB // pattern is applied later 
      case n => n
    }
  }
}

// TODO complete
case class LCarriageSettings() extends CarriageSettings {
  def carriage = LCarriage
  
  def applyOnNeedles(dir: Direction)(before: NeedlePatternRow) = before andThen {
    case NeedleE => throw new IllegalStateException("LCarriage does not work with needles at E")
    case NeedleD => NeedleB //pattern is applied later
    case n => n
  }
}

// TODO complete
case class GCarriageSettings() extends CarriageSettings {
  def carriage = GCarriage
  
  def applyOnNeedles(dir: Direction)(before: NeedlePatternRow) = before andThen {
    case NeedleE => throw new IllegalStateException("GCarriage does not work with needles at E")
    case NeedleD => throw new IllegalStateException("GCarriage does not work with needles at D")
    case n => n
  }
}