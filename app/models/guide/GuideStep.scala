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
        case (StepState(KnitRow(_, direction, _), before, after), i) =>
          val remaining = knittingSteps.size - i - 1
          Instruction(m("knitRow.instruction", direction, remaining),
            before, after)
      }
      (GuideStep(
        m("knitRow.title", direction, knittingSteps.size, carriage),
        m("knitRow.description", direction, knittingSteps.size, carriage),
        instructions,
        knittingSteps.head.before, knittingSteps.last.after),
        tail)

    case ClosedCastOn(bed, from, to, yarn) =>
      guideStep(steps, "closedCastOn", bed, from, to, yarn)
    case ClosedCastOff(bed, yarn, _) =>
      guideStep(steps, "closedCastOff", bed, yarn)

    case AddCarriage(carriage, at) =>
      guideStep(steps, "addCarriage", carriage, at)

    case ThreadYarnK(Some(yarn), None) =>
      guideStep(steps, "threadYarn.k.A", yarn)
    case ThreadYarnK(None, Some(yarn)) =>
      guideStep(steps, "threadYarn.k.B", yarn)
    case ThreadYarnK(None, None) =>
      guideStep(steps, "threadYarn.k.none")
    case ThreadYarnK(Some(yarnA), Some(yarnB)) =>
      guideStep(steps, "threadYarn.k.both", yarnA, yarnB)


    //TODO Rest of the steps
  }

  private def guideStep(steps: Seq[StepState], key: String, args: Any*) = {
    (GuideStep(m(s"$key.title", args: _*), m(s"$key.description", args: _*), steps.head),
      steps.tail)
  }

  private def m(key: String, args: Any*): Text = new Text {
    override def apply(implicit lang: Lang) = {
      val renderedArgs = args map render
      Messages(s"guide.step.$key", renderedArgs: _*)
    }
  }
  private def render(arg: Any)(implicit lang: Lang): Any = arg match {
    case t: Text => t(lang)
    case i: Int => i
    case l: Long => l
    case n: Needle => n.index
    case MainBed => Messages("bed.mainBed")
    case DoubleBed => Messages("bed.doubleBed")
    case c: Carriage => c.name
    case Yarn(name, _) => name
    case piece: YarnPiece => render(piece.yarn)
    case ToLeft => Messages("direction.toLeft")
    case ToRight => Messages("direction.toRight")
    case Left => Messages("leftRight.left")
    case Right => Messages("leftRight.right")
  }
}



