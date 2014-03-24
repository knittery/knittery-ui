package models.guide

import scala.annotation.tailrec
import models._
import models.plan._
import play.api.i18n.{Messages, Lang}


case class GuideStep(title: Text, description: Text,
                     instructions: Seq[Instruction],
                     before: KnittingState, after: KnittingState)
object GuideStep {
  def apply(title: Text, description: Text, step: StepState): GuideStep = {
    GuideStep(title, description,
      Instruction(description, step.before, step.after) :: Nil,
      step.before, step.after)
  }
}

case class Instruction(text: Text,
                       before: KnittingState, after: KnittingState)


object GuideParser {
  def apply(plan: Plan) = {
    @tailrec
    def parseRecursive(toParse: Seq[StepState], acc: Seq[GuideStep]): Seq[GuideStep] = {
      if (toParse.isEmpty) acc
      else {
        val (step, rest) = parse(toParse)
        parseRecursive(rest, acc :+ step)
      }
    }
    parseRecursive(plan.stepStates, Vector.empty)
  }

  private def parse(steps: Seq[StepState]): (GuideStep, Seq[StepState]) = steps.head.step match {
    case KnitRow(carriage, direction, _) =>
      val (knittingSteps, tail) = steps.span {
        case StepState(KnitRow(`carriage`, _, _), _, _) => true
        case _ => false
      }
      val instructions = knittingSteps.zipWithIndex.map {
        case (StepState(_, before, after), i) =>
          val remaining = knittingSteps.size - i - 1
          Instruction(m("knitRow.instruction", direction, remaining),
            before, after)
      }
      (GuideStep(
        m("knitRow.title", direction, knittingSteps.size),
        m("knitRow.description", direction, knittingSteps.size),
        instructions,
        knittingSteps.head.before, knittingSteps.last.after),
        tail)

    case ClosedCastOn(bed, from, to, yarn) =>
      guideStep(steps, "closedCastOn", bed, from, to, yarn.yarn.name)
    case ClosedCastOff(bed, yarn, _) =>
      guideStep(steps, "closedCastOff", bed, yarn.yarn.name)

    //TODO Rest of the steps
  }

  private def guideStep(steps: Seq[StepState], key: String, args: Any*) = {
    (GuideStep(m(s"$key.title", args), m(s"$key.description", args), steps.head),
      steps.tail)
  }

  private def m(key: String, args: Any*): Text = { implicit lang: Lang =>
    val stringArgs = args map {
      case t: Text => t(lang)
      case i: Int => i.toString
      case l: Long => l.toString
      case n: Needle => n.index.toString
      case MainBed => Messages("bed.mainBed")
      case DoubleBed => Messages("bed.doubleBed")
      case ToLeft => Messages("direction.toLeft")
      case ToRight => Messages("direction.toRight")
      case Left => Messages("leftRight.left")
      case Right => Messages("leftRight.right")
    }
    Messages(s"guide.step.$key", stringArgs)
  }
}



