package models.plan

import scalaz._
import Scalaz._
import scalaz.Validation.FlatMap._

case class StepState(step: Step, before: KnittingState) {
  def after: KnittingState = afterOrError.valueOr(e => throw new IllegalStateException(s"invalid plan: $e"))
  def isValid = afterOrError.isSuccess
  lazy val afterOrError: Validation[String, KnittingState] = {
    try {
      step(before)
    } catch {
      case e: NotImplementedError => "not implemented".failure
    }
  }
}
object StepState {
  def stream(stateBefore: KnittingState, steps: Seq[Step]): Stream[StepState] = steps match {
    case Seq(step, rest@_*) =>
      val stepState = StepState(step, stateBefore)
      stepState #:: stream(stepState.after, rest)
    case Seq() => Stream.empty
  }
}

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

private case class CompositePlan private(previous: Plan, stepState: StepState) extends Plan {
  def step = stepState.step
  def run = stepState.after
  override def stepStates = previous.stepStates :+ stepState
}
private object CompositePlan {
  def apply(previous: Plan, step: Step): Validation[PlanError, CompositePlan] = {
    val stepState = StepState(step, previous.run)
    stepState.afterOrError.fold(
      e => PlanError(step, previous.steps.size + 1, e).failure,
      _ => CompositePlan(previous, stepState).success
    )
  }
  def apply(previous: Plan, steps: Seq[Step]): Validation[PlanError, Plan] = {
    steps.foldLeft(previous.success[PlanError]) { (plan, step) =>
      plan.flatMap(CompositePlan(_, step))
    }
  }
  def fromStepState(previous: Plan, step: StepState) = {
    require(previous.run == step.before, "Cannot add step state to plan because states do not match.")
    CompositePlan(previous, step)
  }
  def fromStepStates(previous: Plan, steps: Seq[StepState]): Plan = {
    steps.foldLeft(previous) {
      (plan, step) =>
        require(plan.run == step.before, "Illegal sequence of StepState (state.after of previous state not equal to state.before of this step)")
        CompositePlan(plan, step)
    }
  }
  def fromStepStates(steps: Seq[StepState]): Plan = fromStepStates(StartPlan, steps)
}

