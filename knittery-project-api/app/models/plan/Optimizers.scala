package models.plan

import scala.annotation.tailrec
import scalaz._
import Scalaz._
import scalaz.Validation.FlatMap._
import models._

object Optimizers {
  val list =
    NoEffectStepOptimizer ::
      OptimizePatternKnitting ::
      NoEffectStepOptimizer ::
      OptimizeStepWithNoEffectOnFinalOutput ::
      OptimizeRetires ::
      Nil
  implicit val all = list.foldLeft(Monoid[PlanOptimizer].zero)(_ |+| _)
  implicit def no = Monoid[PlanOptimizer].zero
}

/** Optimizes away steps that do have same input state as output. */
object NoEffectStepOptimizer extends PlanOptimizer {
  override def apply(plan: Plan): Plan = {
    val stepStates = plan.stepStates.filterNot(s => s.before == s.after && !NonOptimizable(s.step))
    CompositePlan.fromStepStates(stepStates)
  }
}

/** Optimizes away steps that have no impact on the result of the knitting. */
object OptimizeStepWithNoEffectOnFinalOutput extends PlanOptimizer {
  /** if the state is still different after this amount of steps it is considered non optimizable. */
  val maxDepth = 20

  override def apply(originalPlan: Plan) = optimize(StartPlan, originalPlan.stepStates.toStream, originalPlan.steps)

  @tailrec
  private def optimize(plan: Plan, stepStates: Stream[StepState], steps: Seq[Step]): Plan = stepStates match {
    case Stream.Empty => plan

    case step #:: Stream.Empty if NonOptimizable(step.step) =>
      CompositePlan.fromStepState(plan, step)
    case step #:: Stream.Empty =>
      if (step.before.sameOutput(step.after)) plan
      else CompositePlan.fromStepState(plan, step)

    case step #:: rest if NonOptimizable(step.step) =>
      optimize(CompositePlan.fromStepState(plan, step), rest, steps.tail)
    case step #:: rest =>
      val withoutThis = StepState.stream(step.before, steps.tail)
      if (withoutThis.nonEmpty && noEffect(withoutThis, rest)) {
        val newPlan = CompositePlan.fromStepState(plan, withoutThis.head)
        optimize(newPlan, withoutThis.tail, steps.tail.tail)
      } else {
        val newPlan = CompositePlan.fromStepState(plan, step)
        optimize(newPlan, rest, steps.tail)
      }
  }

  private def noEffect(as: Stream[StepState], bs: Stream[StepState]): Boolean = {
    as.zip(bs).take(maxDepth).flatMap {
      case (a, b) =>
        assert(a.step == b.step, s"one of the following steps ${a.step} != ${b.step}")
        if (!a.isValid || !a.after.sameOutput(b.after)) Some(false)
        else if (a.after.sameState(b.after)) Some(true)
        else None
    }.headOption.getOrElse(false)
  }
}

/** Optimizes away manual MoveNeedles steps. */
object OptimizePatternKnitting extends PlanOptimizer {
  override def apply(plan: Plan) = {
    plan.stepStates.foldLeft(Acc(StartPlan, None))(_ + _).result
  }

  private case class Acc(plan: Plan, window: Option[OptimizationWindow]) {
    def +(step: StepState) = {
      step.step match {
        case knit: KnitRow =>
          val pushed = pushWindow
          val window = OptimizationWindow(knit, Vector.empty, appendToPlan(pushed.plan, step))
          pushed.copy(window = Some(window))
        case _: ClosedCastOn => pushWindow.append(step)
        case _: ClosedCastOff => pushWindow.append(step)
        case _ =>
          window.map(w => copy(window = Some(w + step))).
            getOrElse(append(step))

      }
    }
    def result = pushWindow.plan
    private def pushWindow = window match {
      case Some(window) => copy(plan = window.result, window = None)
      case None => this
    }
    private def appendToPlan(appendTo: Plan, step: StepState) = {
      if (appendTo.run == step.before) CompositePlan.fromStepState(appendTo, step)
      else CompositePlan(appendTo, step.step).valueOr(e => throw new RuntimeException(s"Invalid plan: $e"))
    }
    private def append(step: StepState) = {
      assert(window.isEmpty)
      copy(plan = appendToPlan(plan, step))
    }
  }
  private case class OptimizationWindow(knit: KnitRow, optimizable: Seq[StepState], planAfterKnit: CompositePlan) {
    def +(step: StepState) = copy(optimizable = optimizable :+ step)
    def result: Plan = {
      val (afterR, toOptimizeR) = optimizable.reverse.span(!isMoveNeedlesOnMainBed(_))
      if (toOptimizeR.isEmpty) unoptimized
      else {
        //try to optimize
        val moveNeedles = toOptimizeR.head.step.asInstanceOf[MoveNeedles]
        val otherSteps = toOptimizeR.filterNot(isMoveNeedlesOnMainBed).reverse.map(_.step) ++ afterR.map(_.step).reverse
        def patternActions(n: Needle) = if (moveNeedles.to(n) == NeedleD) NeedleToD else NeedleToB
        val modKnit = knit.copy(pattern = patternActions)
        modKnit(planAfterKnit.previous.run).flatMap { stateModKnit =>
          val delta = movedNeedles(stateModKnit, toOptimizeR.head.after)
          def moved = movedNeedles(toOptimizeR.head.before, toOptimizeR.head.after)
          if (delta.isEmpty) {
            //optimized it away, pattern knitting can do it for us
            CompositePlan(planAfterKnit.previous, modKnit +: otherSteps)
          } else if (delta.size < moved.size) {
            // could not optimize completely, but it helps at least with some needles
            CompositePlan(planAfterKnit.previous, modKnit +: otherSteps :+ moveNeedles)
          } else {
            // could not optimize
            unoptimized.success
          }
        }.valueOr(_ => unoptimized)
      }
    }
    private def unoptimized = CompositePlan.fromStepStates(planAfterKnit, optimizable)
    private def movedNeedles(s1: KnittingState, s2: KnittingState) =
      Needle.all.filterNot(n => s1.needles(MainBed)(n) == s2.needles(MainBed)(n))
    private def isMoveNeedlesOnMainBed(s: StepState) = s match {
      case StepState(MoveNeedles(MainBed, _), _) => true
      case _ => false
    }
  }
}

/** Optimize needle retirement by making use on the double and triple decker. */
object OptimizeRetires extends PlanOptimizer {
  def apply(plan: Plan) = {
    val segmented = plan.stepStates.foldLeft[List[Segment]](Nil) {
      case (RetireSegment(retires, before) :: acc, StepState(step: RetireNeedle, _)) =>
        RetireSegment(retires :+ step, before) :: acc
      case (acc, StepState(step: RetireNeedle, before)) =>
        RetireSegment(Seq(step), before) :: acc
      case (acc, otherStep) => OtherSegment(otherStep) :: acc
    }.reverse
    CompositePlan.fromStepStates(segmented.flatMap(_.optimize))
  }

  private sealed trait Segment {
    def optimize: Seq[StepState]
  }
  private case class RetireSegment(retires: Seq[RetireNeedle], before: KnittingState) extends Segment {
    def optimize: Seq[StepState] = {
      opt(retires, before, Seq.empty)
    }
    private def overlap(a: RetireNeedle)(b: RetireNeedle): Boolean = {
      a.at == b.target && a.bed == b.bed
    }
    @tailrec
    private def opt(todo: Seq[RetireNeedle], state: KnittingState, result: Seq[StepState]): Seq[StepState] = todo match {
      case Seq(step@RetireNeedle(bed, at, ToLeft), rest@_*) =>
        val (l, r) = rest.span(s => !(s.bed == bed && s.at == at + 1))
        val (step2, rest2) = if (r.nonEmpty && !l.exists(overlap(step))) {
          //try to find third
          val (l2, r2) = r.drop(1).span(s => !(s.bed == bed && s.at == at + 2))
          if (r2.nonEmpty && !l2.exists(overlap(step)) && !l2.exists(overlap(r.head))) {
            //can use triple-decker
            (RetireWithTriple(bed, at, ToLeft), l ++ l2.drop(1) ++ r2.drop(1))
          } else {
            //can use double-decker
            (RetireWithDouble(bed, at, ToLeft), l ++ r.drop(1))
          }
        } else {
          (step, rest)
        }
        val ss = StepState(step2, state)
        opt(rest2, ss.after, result :+ ss)

      case Seq(step@RetireNeedle(bed, at, ToRight), rest@_*) =>
        val (l, r) = rest.span(s => !(s.bed == bed && s.at == at - 1))
        val (step2, rest2) = if (r.nonEmpty && !l.exists(overlap(step))) {
          //try to find third
          val (l2, r2) = r.drop(1).span(s => !(s.bed == bed && s.at == at - 2))
          if (r2.nonEmpty && !l2.exists(overlap(step)) && !l2.exists(overlap(r.head))) {
            //can use triple-decker
            (RetireWithTriple(bed, r2.head.at, ToRight), l ++ l2.drop(1) ++ r2.drop(1))
          } else {
            //can use double-decker
            (RetireWithDouble(bed, r.head.at, ToRight), l ++ r.drop(1))
          }
        } else {
          (step, rest)
        }
        val ss = StepState(step2, state)
        opt(rest2, ss.after, result :+ ss)

      case Seq() => result
    }
  }
  private case class OtherSegment(step: StepState) extends Segment {
    def optimize = Seq(step)
  }
}