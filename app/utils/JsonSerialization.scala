package utils

import play.api.libs.json._
import models._
import models.machine.Machine._
import models.guide._

object JsonSerialization {

  implicit object CarriageTypeWrite extends Writes[CarriageType] {
    override def writes(carriage: CarriageType) = JsString(carriage match {
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

  implicit object GuideStepWrite extends Writes[GuideStep] {
    override def writes(step: GuideStep) = {
      Json.obj("name" -> step.name,
        "description" -> step.description,
        "number" -> step.stepNumber)
    }
  }
}