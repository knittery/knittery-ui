package utils

import java.awt.Color
import play.api.libs.json._
import models._
import models.machine.Machine._
import models.guide._
import models.plan._
import ch.inventsoft.graph.layout.Position

object JsonSerialization {

  implicit object CarriageTypeWrite extends Writes[Carriage] {
    override def writes(carriage: Carriage) = JsString(carriage match {
      case KCarriage => "K"
      case LCarriage => "L"
      case GCarriage => "G"
    })
  }

  implicit object CarriagePositionWrite extends Writes[CarriagePosition] {
    override def writes(pos: CarriagePosition) = pos match {
      case CarriageLeft(i) => Json.obj("where" -> "left", "overlap" -> i)
      case CarriageRight(i) => Json.obj("where" -> "right", "overlap" -> i)
      case CarriageOverNeedles(i) => Json.obj("where" -> "needles", "needle" -> i.number, "index" -> i.index)
      case CarriageRemoved => Json.obj("where" -> "removed")
    }
  }

  implicit object PositionChangedWrite extends Writes[PositionChanged] {
    override def writes(event: PositionChanged) = {
      Json.obj("event" -> "positionChange",
        "carriage" -> event.carriage,
        "position" -> event.position,
        "row" -> event.row)
    }
  }

  implicit object NeedlePatternRowWrite extends Writes[NeedlePatternRow] {
    override def writes(row: NeedlePatternRow) = {
      val values = row.all.map {
        case NeedleA => "A"
        case NeedleB => "B"
        case NeedleD => "D"
        case NeedleE => "E"
      }.mkString
      JsString(values)
    }
  }

  implicit object ColorWrite extends Writes[Color] {
    override def writes(color: Color) = {
      val value = color.getRGB | 0xff000000
      JsString("#" + value.toHexString.drop(2))
    }
  }
  implicit object PositionWrite extends Writes[Position] {
    override def writes(pos: Position) = Json.obj(
      "x" -> BigDecimal(pos.x), "y" -> BigDecimal(pos.y), "z" -> BigDecimal(pos.z))
  }
  implicit object YarnWrite extends Writes[Yarn] {
    override def writes(yarn: Yarn) = Json.obj("name" -> yarn.name, "color" -> yarn.color)
  }

  implicit object StitchWrite extends Writes[Stitch] {
    override def writes(stitch: Stitch) = stitch match {
      case NoStitch => Json.obj("type" -> "no")
      case EmptyStitch => Json.obj("type" -> "empty")
      case PlainStitch(yarns) => Json.obj("type" -> "plain", "yarns" -> yarns)
      case PurlStitch(yarns) => Json.obj("type" -> "purl", "yarns" -> yarns)
      case CastOnStitch(yarns) => Json.obj("type" -> "castOn", "yarns" -> yarns)
      case CastOffStitch(yarns) => Json.obj("type" -> "castOff", "yarns" -> yarns)
    }
  }

  implicit object KnittedWrite extends Writes[Knitted] {
    override def writes(knitted: Knitted) = Json.toJson(knitted.rows)
  }

  implicit object HoldingCamLeverWrite extends Writes[KCarriage.HoldingCamLever] {
    override def writes(holdingCamLever: KCarriage.HoldingCamLever) = JsString(holdingCamLever match {
      case KCarriage.HoldingCamH => "H"
      case KCarriage.HoldingCamI => "I"
      case KCarriage.HoldingCamN => "N"
    })
  }

  implicit object KCarriageSettingWrite extends Writes[KCarriage.Settings] {
    override def writes(settings: KCarriage.Settings) = {
      Json.obj("mc" -> settings.mc,
        "l" -> settings.l,
        "partLeft" -> settings.partLeft,
        "partRight" -> settings.partRight,
        "tuckLeft" -> settings.tuckLeft,
        "tuckRight" -> settings.tuckRight,
        "holdingCamLever" -> settings.holdingCamLever)
    }
  }

  implicit object KCarriageDoubleBedWrite extends Writes[KCarriage.DoubleBedCarriage] {
    override def writes(settings: KCarriage.DoubleBedCarriage) = {
      Json.obj(
        "partLeft" -> settings.partLeft,
        "partRight" -> settings.partRight,
        "needleTakebackLeft" -> settings.needleTakebackLeft,
        "needleTakebackRight" -> settings.needleTakebackRight)
    }
  }

  implicit object CarriageStatesWrite extends Writes[CarriageStates] {
    override def writes(states: CarriageStates) = {
      val base = Json.obj(
        "k" -> states(KCarriage).settings
      )
      states(KCarriage).assembly match {
        case dbc: KCarriage.DoubleBedCarriage =>
          base deepMerge Json.obj("doubleBed" -> dbc)
        case z: KCarriage.SinkerPlate =>
          // TODO add to carriages / "k"
          base
        case _ => base
      }
    }
  }

  implicit object KnittingStateWrite extends Writes[KnittingState] {
    override def writes(state: KnittingState) = {
      Json.obj("needles" -> state.needles(MainBed).pattern,
        "doubleBedNeedles" -> state.needles(DoubleBed).pattern,
        "output" -> state.output,
        "carriage" -> state.carriageState)
    }
  }

  implicit object GuideStepWrite extends Writes[GuideStep] {
    override def writes(step: GuideStep) = {
      Json.obj("name" -> step.name,
        "description" -> step.description,
        "number" -> step.stepNumber,
        "isKnitting" -> step.isKnitting,
        "manualNeedles" -> step.manualNeedles(MainBed).map(_.index),
        "manualDoubleBedNeedles" -> step.manualNeedles(DoubleBed).map(_.index),
        "stateBefore" -> step.stateBefore,
        "stateAfter" -> step.stateAfter)
    }
  }
}