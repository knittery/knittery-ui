package models.plan

import scalaz._
import Scalaz._
import models._

case class StepState(step: Step, before: KnittingState, after: KnittingState)
case class PlanError(atStep: Step, stepNumber: Int, error: String)

sealed trait Plan {
  def steps: Seq[Step] = stepStates.map(_.step)
  def stepStates: Seq[StepState]
  def run: KnittingState
}
object Plan {
  def apply(steps: Seq[Step]) = CompositePlan(StartPlan, steps)
}

private case object StartPlan extends Plan {
  override def stepStates = Vector.empty
  override def run = KnittingState.initial
}

private case class CompositePlan private(previous: Plan, step: Step, run: KnittingState) extends Plan {
  def stepState = StepState(step, previous.run, run)
  override def stepStates = previous.stepStates :+ stepState
}
private object CompositePlan {
  def apply(previous: Plan, step: Step): Validation[PlanError, CompositePlan] = {
    try {
      step(previous.run).map(state => CompositePlan(previous, step, state))
        .leftMap(e => PlanError(step, previous.steps.size + 2, e))
    } catch {
      case e: NotImplementedError => PlanError(step, previous.steps.size + 2, s"not implemented: $e").fail[CompositePlan]
    }
  }
  def apply(previous: Plan, steps: Seq[Step]): Validation[PlanError, Plan] = {
    steps.foldLeft(previous.success[PlanError]) { (plan, step) =>
      plan.flatMap(CompositePlan(_, step))
    }
  }
  def fromStepState(previous: Plan, step: StepState) = {
    require(previous.run == step.before, "Cannot add step state to plan because states do not match.")
    CompositePlan(previous, step.step, step.after)
  }
  def fromStepStates(previous: Plan, steps: Seq[StepState]): Plan = {
    steps.foldLeft(previous) {
      (plan, step) =>
        require(plan.run == step.before, "Illegal sequence of StepState (state.after of previous state not equal to state.before of this step)")
        CompositePlan(plan, step.step, step.after)
    }
  }
  def fromStepStates(steps: Seq[StepState]): Plan = fromStepStates(StartPlan, steps)
}

