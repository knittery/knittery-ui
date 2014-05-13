package models.guide

import scala.annotation.tailrec
import models._
import models.plan._
import play.api.i18n.{Messages, Lang}


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
    val steps = parseRecursive(plan.stepStates, Vector.empty)
    GuideStep.updatePos(steps)
  }

  private def parse(steps: Seq[StepState]): (GuideStep, Seq[StepState]) = steps.head.step match {
    case KnitRow(carriage, direction, _) =>
      val (knittingSteps, tail) = steps.span {
        case StepState(KnitRow(`carriage`, _, _), _, _) => true
        case _ => false
      }
      val instructions = knittingSteps.zipWithIndex.map {
        case (StepState(step@KnitRow(_, direction, _), before, after), i) =>
          val remaining = knittingSteps.size - i
          Instruction(m("knitRow.instruction", direction, remaining), step,
            Set.empty, before, after)
      }
      (GuideStep(
        m("knitRow.title", direction, knittingSteps.size, carriage),
        m("knitRow.description", direction, knittingSteps.size, carriage),
        instructions, true,
        knittingSteps.head.before, knittingSteps.last.after),
        tail)

    case c@ClosedCastOn(bed, from, to, yarn) =>
      val affects = c.needles.map((bed, _)).toSet
      guideStepWithMark(steps, "closedCastOn", affects, bed, from, to, yarn)
    case ClosedCastOff(bed, yarn, filter) =>
      val affects = Needle.all.filter(filter).map((bed, _)).toSet
      guideStepWithMark(steps, "closedCastOff", affects, bed, yarn)

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

    case ThreadYarnG(Some(yarn)) =>
      guideStep(steps, "threadYarn.g.one", yarn)
    case ThreadYarnG(None) =>
      guideStep(steps, "threadYarn.g.none")

    case MoveNeedles(bed, to) =>
      val affects = Needle.all.
        filter(n => steps.head.before.needles(bed)(n).position != to(n)).
        map((bed, _)).toSet
      guideStepWithMark(steps, "moveNeedles", affects, bed)

    case MoveToDoubleBed(_, offset, None) =>
      guideStep(steps, "moveToDoubleBed.noflip", offset)
    case MoveToDoubleBed(_, offset, Some(flip)) =>
      guideStep(steps, "moveToDoubleBed.flip", offset, flip)

    case s@RetireNeedle(bed, needle, direction) =>
      guideStep(steps, "retireNeedle", needle, direction, s.target)

    case HangOnCastOnComb() =>
      guideStep(steps, "hangOnCastOnComb")

    case ChangeKCarriageSettings(settings, assembly) =>
      val assemblyBefore = steps.head.before.carriageState(KCarriage).assembly
      def settingsBefore = steps.head.before.carriageState(KCarriage).settings
      assembly match {
        case db: KCarriage.DoubleBedCarriage =>
          if (assemblyBefore.isInstanceOf[KCarriage.DoubleBedCarriage]) {
            if (assembly == assemblyBefore) {
              if (settings == settingsBefore) guideStep(steps, "changeSettings.k.double")
              else guideStep(steps, "changeSettings.k.both")
            } else guideStep(steps, "changeSettings.k.main")
          } else guideStep(steps, "changeSettings.k.addDouble")
        case sp: KCarriage.SinkerPlate =>
          if (assemblyBefore.isInstanceOf[KCarriage.DoubleBedCarriage]) guideStep(steps, "changeSettings.k.removeDouble")
          else guideStep(steps, "changeSettings.k.main")
      }

    case ChangeLCarriageSettings(settings) =>
      guideStep(steps, "changeSettings.l")

    case ChangeGCarriageSettings(settings) =>
      guideStep(steps, "changeSettings.g")
  }

  private def guideStep(steps: Seq[StepState], key: String, args: Any*) = {
    val step = steps.head
    val desc = m(s"$key.description", args: _*)
    (
      GuideStep(m(s"$key.title", args: _*), desc,
        Instruction(desc, step.step, Set.empty, step.before, step.after) :: Nil,
        false, step.before, step.after),
      steps.tail)
  }
  private def guideStepWithMark(steps: Seq[StepState], key: String, mark: Set[(Bed, Needle)], args: Any*) = {
    val (base, tail) = guideStep(steps, key, args: _*)
    (base.copy(instructions = base.instructions.head.copy(markNeedles = mark) :: Nil), tail)
  }

  private def m(key: String, args: Any*): Text = new Text {
    override def apply(lang: Lang) = {
      implicit val l = lang
      val renderedArgs = args map render
      Messages(s"guide.step.$key", renderedArgs: _*)
    }
  }
  private def render(arg: Any)(implicit lang: Lang): Any = arg match {
    case t: Text => t(lang)
    case i: Int => i
    case l: Long => l
    case n: Needle => n.number
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
