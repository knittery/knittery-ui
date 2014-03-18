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

private case object StartPlan extends Plan {
  override def stepStates = Vector.empty
  override def run = KnittingState.initial
}

private case class CompositePlan private(previous: Plan, step: Step, run: KnittingState) extends Plan {
  def stepState = StepState(step, previous.run, run)
  def stepStates = previous.stepStates :+ stepState
}
private object CompositePlan {
  def apply(previous: Plan, step: Step): Validation[PlanError, Plan] = {
    step(previous.run).map(CompositePlan(previous, step, _)).
      leftMap(e => PlanError(step, previous.steps.size + 2, e))
  }
}

