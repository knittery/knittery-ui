package models.plan

import scala.util.Try
import scalaz._
import Scalaz._
import models._
import utils._

private trait KnittingCarriage {
  def apply(direction: Direction, needles: NeedleStateRow): Validation[String, (NeedleStateRow, Needle => Stitch)]
}

private object KnittingCarriage {
  def apply(carriageState: CarriageState,
    pattern: NeedleActionRow) = carriageState match {
    case state: KCarriage.State =>
      new KKnittingCarriage(state.settings, state.yarnA, state.yarnB, pattern)
    case state: LCarriage.State =>
      new LKnittingCarriage(state.settings, pattern)
    case state: GCarriage.State =>
      new GKnittingCarriage(state.settings, state.yarn, pattern)
  }

  private class KKnittingCarriage(settings: KCarriage.Settings,
    yarnA: Option[YarnFlow], yarnB: Option[YarnFlow],
    pattern: NeedleActionRow)
    extends KnittingCarriage {
    import KCarriage._

    override def apply(direction: Direction, needles: NeedleStateRow) = Try {
      //Basic movement of needles and putting yarn on needles
      val baseState = (needle: Needle) => {
        //TODO part modes
        //TODO tuck modes
        //TODO l mode
        //TODO holdingCamI
        needles(needle).position match {
          case NeedleA => NeedleState(NeedleA)
          case NeedleB => NeedleState(NeedleB, yarnA)
          case NeedleD => NeedleState(NeedleB, if (settings.mc) yarnB else yarnA)
          case NeedleE if settings.holdingCamLever == HoldingCamN => NeedleState(NeedleB, yarnA)
          case NeedleE => NeedleState(NeedleE, yarnA)
        }
      }
      //Apply the pattern (moves Needles between B and D). 
      val patternState = (needle: Needle) => {
        val s @ NeedleState(pos, yarn) = baseState(needle)
        if (pos == NeedleB || pos == NeedleD)
          s.copy(position = pattern(needle).toPosition)
        else s
      }
      //Knitting performed
      def knitted(n: Needle) = needles(n) match {
        //TODO part modes
        //TODO tuck modes
        //TODO l mode
        case NeedleState(NeedleB, yarns) if yarns.nonEmpty => PlainStitch(yarns.map(_.yarn))
        case NeedleState(NeedleD, yarns) if yarns.nonEmpty => PlainStitch(yarns.map(_.yarn))
        case NeedleState(NeedleE, yarns) if yarns.nonEmpty && settings.holdingCamLever == HoldingCamN =>
          PlainStitch(yarns.map(_.yarn))
        case NeedleState(NeedleE, _) => NoStitch
        case other => EmptyStitch
      }
      (patternState, knitted _)
    }
  }

  private class LKnittingCarriage(settings: LCarriage.Settings, pattern: NeedleActionRow)
    extends KnittingCarriage {

    def apply(direction: Direction, needles: NeedleStateRow) = Try {
      if (needles.pattern.all.exists(_ == NeedleE))
        throw new IllegalStateException("LCarriage does not work with needles at E")

      //TODO implement L knitting
      ???
    }
  }

  private class GKnittingCarriage(settings: GCarriage.Settings, yarnA: Option[YarnFlow], pattern: NeedleActionRow)
    extends KnittingCarriage {

    def apply(direction: Direction, needles: NeedleStateRow) = Try {
      if (needles.pattern.all.exists(_ == NeedleE))
        throw new IllegalStateException("GCarriage does not work with needles at E")
      if (needles.pattern.all.exists(_ == NeedleD))
        throw new IllegalStateException("GCarriage does not work with needles at D")

      //TODO implement G knitting
      ???
    }
  }
}