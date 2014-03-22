package models.plan

import scala.annotation.tailrec
import scalaz._
import Scalaz._
import models._
import scala.util.Try

object Optimizers {
  val list =
    NoEffectStepOptimizer ::
      OptimizePatternKnitting ::
      NoEffectStepOptimizer ::
      OptimizeStepWithNoEffectOnFinalOutput ::
      Nil
  implicit val all = list.foldLeft(Monoid[PlanOptimizer].zero)(_ |+| _)
  implicit def no = Monoid[PlanOptimizer].zero
}


/** Optimizes away steps that do have same input state as output. */
object NoEffectStepOptimizer extends PlanOptimizer {
  override def apply(plan: Plan): Plan = {
    val stepStates = plan.stepStates.filterNot(s => s.before == s.after)
    CompositePlan.fromStepStates(stepStates)
  }
}

/** Optimizes away steps that have no impact on the result of the knitting. */
object OptimizeStepWithNoEffectOnFinalOutput extends PlanOptimizer {
  override def apply(plan: Plan): Plan = {
    val expectedResult = plan.run
    plan.steps.tails.foldLeft((StartPlan: Plan, plan)) {
      case ((soFar, proposed), steps) if steps.size > 0 =>
        makePlan(soFar, steps.tail) match {
          case Success(withoutThisStep) if withoutThisStep.run == expectedResult =>
            (soFar, withoutThisStep)
          case differentResult =>
            val next = proposed.stepStates.drop(soFar.stepStates.size).head
            val withThisStep = CompositePlan.fromStepState(soFar, next)
            assert(next.step == steps.head)
            (withThisStep, proposed)
        }
      case (r, _) => r
    }._1
  }
  private def makePlan(previous: Plan, steps: Seq[Step]) = {
    try {
      CompositePlan(previous, steps)
    }
    catch {
      case e: Error => "not implemented".fail[Plan]
    }
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
      case StepState(MoveNeedles(MainBed, _), _, _) => true
      case _ => false
    }
  }
}