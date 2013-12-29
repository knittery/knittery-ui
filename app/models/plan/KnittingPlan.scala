package models.plan

import scalaz._
import Scalaz._
import models._

case class KnittingPlan(steps: Seq[KnittingStep]) {
  def +(other: KnittingPlan) = KnittingPlan(steps ++ other.steps)

  /** Run the knitting plan to produce the final knitting state. */
  def run: Validation[KnittingPlanError, KnittingState] =
    run(KnittingState.initial)

  def run(state: KnittingState) = {
    steps.zipWithIndex.
      foldLeft(state.success[KnittingPlanError]) {
        case (state, (step, index)) => state.flatMap { st =>
          step(st).leftMap(e => KnittingPlanError(step, index + 1, e))
        }
      }
  }
}

case class KnittingPlanError(atStep: KnittingStep, stepNumber: Int, error: String)