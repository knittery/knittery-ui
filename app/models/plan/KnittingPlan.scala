package models.plan

import scalaz._
import Scalaz._
import models._

case class KnittingPlan(steps: Seq[KnittingStep]) {
  def +(other: KnittingPlan) = KnittingPlan(steps ++ other.steps)

  def validate: Option[KnittingPlanError] = {
    def validateSteps(state: KnittingState, remaining: List[KnittingStep], nr: Int): Option[KnittingPlanError] = remaining match {
      case step :: rest =>
        step(state) match {
          case Success(s2) => validateSteps(s2, rest, nr + 1)
          case Failure(error) => Some(KnittingPlanError(step, nr, error))
        }
      case Nil => None
    }
    validateSteps(KnittingState.initial, steps.toList, 1)
  }
}

case class KnittingPlanError(atStep: KnittingStep, stepNumber: Int, error: String)