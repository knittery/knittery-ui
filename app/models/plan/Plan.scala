package models.plan

import scalaz._
import Scalaz._
import models._

sealed trait Plan {
  def steps: Seq[Step]

  /** Run the knitting plan to produce the final knitting state, starts with KnittingState.initial. */
  def run: Validation[PlanError, KnittingState] = run(KnittingState.initial)
  /** Run the knitting plan to produce the final knitting state. */
  def run(state: KnittingState): Validation[PlanError, KnittingState]

  def cache(state: KnittingState): Plan
}

private case object EmptyPlan extends Plan {
  override def steps = Seq.empty
  override def run(state: KnittingState) = state.success
  override def cache(state: KnittingState) = this
}

private case class StepPlan(step: Step) extends Plan {
  override def steps = Vector(step)
  override def run(state: KnittingState) = step(state).leftMap(e => PlanError(step, 1, e))
  override def cache(state: KnittingState) = CachedPlan(this, state)
}

private case class CompositePlan(left: Plan, right: Plan) extends Plan {
  override def steps = left.steps ++ right.steps
  override def run(state: KnittingState) = left.run(state).flatMap { s2 =>
    right.run(s2).leftMap(e => e.copy(stepNumber = left.steps.size + e.stepNumber))
  }
  override def cache(state: KnittingState) = CachedPlan(this, state)
}

private case class CachedPlan(plan: Plan, cachedFor: KnittingState, cachedResult: Validation[PlanError, KnittingState]) extends Plan {
  override def steps = plan.steps
  override def run(state: KnittingState) = {
    //Use object identity because comparing states is rather slow
    // since results will be cached also be previous it will still match
    if (cachedFor eq state) cachedResult
    else plan.run(state)
  }
  override def cache(state: KnittingState) = {
    if (cachedFor eq state) this
    else CachedPlan(this, state)
  }
}
private object CachedPlan {
  def apply(plan: Plan, cacheFor: KnittingState): Plan =
    CachedPlan(plan, cacheFor, plan.run(cacheFor))
}

object Plan {
  def apply(step: Step): Plan = StepPlan(step)
  def apply(step: Seq[Step]) = step.map(StepPlan).foldLeft(Monoid[Plan].zero)(CompositePlan)

  def planMonoid: Monoid[Plan] = new Monoid[Plan] {
    override def zero = EmptyPlan
    override def append(a: Plan, b: => Plan): Plan = {
      if (a == EmptyPlan) b
      else if (b == EmptyPlan) a
      else CompositePlan(a, b)
    }
  }

}

case class PlanError(atStep: Step, stepNumber: Int, error: String)