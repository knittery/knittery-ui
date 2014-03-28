package utils

import java.awt.Color
import play.api.libs.json._
import models._
import models.machine.Machine._
import models.guide._
import models.plan._
import ch.inventsoft.graph.layout.Position
import play.api.i18n.Lang

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
        "position" -> event.position)
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

  implicit object TensionDialWrite extends Writes[KCarriage.TensionDial] {
    override def writes(tension: KCarriage.TensionDial) = {
      Json.obj("number" -> tension.number,
        "thirds" -> tension.thirds,
        "text" -> tension.text)
    }
  }

  implicit object KRChangeKnobWrite extends Writes[KCarriage.KRChangeKnob] {
    override def writes(knob: KCarriage.KRChangeKnob) = knob match {
      case KCarriage.KRChangeKnobIiIi => JsString("IiIi")
      case KCarriage.KRChangeKnobPlain => JsString("plain")
    }
  }

  implicit object SlideLeverWrite extends Writes[KCarriage.SlideLever] {
    override def writes(lever: KCarriage.SlideLever) = lever match {
      case KCarriage.SlideLeverI => JsString("I")
      case KCarriage.SlideLeverII => JsString("II")
      case KCarriage.SlideLeverIiIi => JsString("IiIi")
    }
  }

  implicit object TuckingLeverWrite extends Writes[KCarriage.TuckingLever] {
    override def writes(lever: KCarriage.TuckingLever) = lever match {
      case KCarriage.TuckingLeverR => JsString("R")
      case KCarriage.TuckingLeverP => JsString("P")
    }
  }


  implicit object KCarriageSettingWrite extends Writes[KCarriage.Settings] {
    override def writes(settings: KCarriage.Settings) = {
      Json.obj("tension" -> settings.tension,
        "mc" -> settings.mc,
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
        "tension" -> settings.tension,
        "slideLever" -> settings.slideLever,
        "knobLeft" -> settings.knobLeft,
        "knobRight" -> settings.knobRight,
        "tuckingLever" -> settings.tuckingLever,
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
        "carriage" -> state.carriageState,
        "visibleStitches3D" -> state.output3D.stitches.size)
    }
  }

  def localized(implicit lang: Lang) = new Object {
    implicit object InstructionWrite extends Writes[Instruction] {
      override def writes(instr: Instruction) = {
        Json.obj("text" -> instr.text(lang),
          "index" -> instr.position.index,
          "first" -> instr.position.isFirst,
          "last" -> instr.position.isLast,
          "markNeedlesMainBed" -> instr.markNeedles.filter(_._1 == MainBed).map(_._2.index),
          "markNeedlesDoubleBed" -> instr.markNeedles.filter(_._1 == DoubleBed).map(_._2.index),
          "stateBefore" -> instr.before,
          "stateAfter" -> instr.after)
      }
    }

    implicit object GuideStepWrite extends Writes[GuideStep] {
      override def writes(step: GuideStep) = {
        Json.obj("title" -> step.title(lang),
          "description" -> step.description(lang),
          "instructionCount" -> step.instructions.size,
          "index" -> step.position.index,
          "first" -> step.position.isFirst,
          "last" -> step.position.isLast,
          "stateBefore" -> step.before,
          "stateAfter" -> step.after)
      }
    }
  }
}