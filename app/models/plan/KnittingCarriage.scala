package models.plan

import scala.util.{ Try, Success, Failure }
import models._

trait KnittingCarriage {
  def modifyNeedles(direction: Direction)(before: NeedlePatternRow): Try[NeedlePatternRow]
  def knit(row: NeedlePatternRow, next: NeedlePatternRow): KnittedRow
}
object KnittingCarriage {
  def apply(carriage: CarriageType, settings: CarriageSettings, yarnA: Option[Yarn], yarnB: Option[Yarn]) = {
    require(carriage == settings.carriage, "Setting does not match carriage")
    settings match {
      case s: KCarriageSettings => new KKnittingCarriage(s, yarnA, yarnB)
      case s: LCarriageSettings => new LKnittingCarriage(s)
      case s: GCarriageSettings => new GKnittingCarriage(s, yarnA)
    }
  }

  private class KKnittingCarriage(settings: KCarriageSettings, yarnA: Option[Yarn], yarnB: Option[Yarn])
    extends KnittingCarriage {

    override def modifyNeedles(dir: Direction)(before: NeedlePatternRow) = Try {
      if (settings.knob == KKnobCR) throw new IllegalStateException("Not knitting in CR setting")
      before andThen {
        case NeedleE if settings.holdingCamLever == HoldingCamH => NeedleE
        case NeedleE => NeedleB
        case NeedleD => NeedleB // pattern is applied later 
        case n => n
      }
    }

    override def knit(row: NeedlePatternRow, next: NeedlePatternRow) = {
      ???
    }
  }

  private class LKnittingCarriage(settings: LCarriageSettings)
    extends KnittingCarriage {

    def modifyNeedles(dir: Direction)(before: NeedlePatternRow) = Try {
      if (Needle.all.map(before).exists(_ == NeedleE))
        throw new IllegalStateException("LCarriage does not work with needles at E")
      before andThen {
        case NeedleE => throw new AssertionError("No E needles allowed for LCarriage")
        case NeedleD => NeedleB //pattern is applied later
        case n => n
      }
    }
    def knit(row: NeedlePatternRow, next: NeedlePatternRow) = {
      ??? //TODO implement
    }
  }

  private class GKnittingCarriage(settings: GCarriageSettings, yarnA: Option[Yarn])
    extends KnittingCarriage {

    def modifyNeedles(dir: Direction)(before: NeedlePatternRow) = Try {
      if (Needle.all.map(before).exists(_ == NeedleE))
        throw new IllegalStateException("GCarriage does not work with needles at E")
      if (Needle.all.map(before).exists(_ == NeedleD))
        throw new IllegalStateException("GCarriage does not work with needles at D")
      before
    }
    def knit(row: NeedlePatternRow, next: NeedlePatternRow) = {
      ??? //TODO implement
    }
  }
}