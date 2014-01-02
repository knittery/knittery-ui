package models.plan

import scalaz._
import Scalaz._
import models._

case class Plan(steps: Seq[KnittingStep]) {
  /** Run the knitting plan to produce the final knitting state. */
  def run: Validation[PlanError, KnittingState] =
    run(KnittingState.initial)

  def run(state: KnittingState) = {
    steps.zipWithIndex.
      foldLeft(state.success[PlanError]) {
        case (state, (step, index)) => state.flatMap { st =>
          step(st).leftMap(e => PlanError(step, index + 1, e))
        }
      }
  }
}
object Plan {
  def planMonoid: Monoid[Plan] = new Monoid[Plan] {
    override def zero = Plan(Nil)
    override def append(a: Plan, b: => Plan) = Plan(a.steps ++ b.steps)
  }

}

case class PlanError(atStep: KnittingStep, stepNumber: Int, error: String)