package models.guide

import models._
import models.plan._

class GuideStep private (val step: Step, processedReversed: List[Step], remaining: List[Step]) {
  def next = {
    require(!isLast, "Already at end")
    val (next :: rest) = remaining
    new GuideStep(next, step :: processedReversed, rest)
  }
  def previous = {
    require(!isFirst, "Already at start")
    val (prev :: before) = processedReversed
    new GuideStep(prev, before, step :: remaining)
  }
  def first: GuideStep = if (isFirst) this else previous.first
  def isFirst = processedReversed.isEmpty
  def last: GuideStep = if (isLast) this else next.last
  def isLast = remaining.isEmpty

  /** Number of this step (1-based). */
  def stepNumber = processedReversed.size + 1

  def stateAfter: KnittingState = {
    step(stateBefore).valueOr(e => throw InvalidPlanException(PlanError(step, stepNumber, e)))
  }
  def stateBefore: KnittingState = {
    if (isFirst) KnittingState.initial
    else previous.stateAfter
  }

  override def toString = s"GuideStep($step)"
}

object GuideStep {
  def apply(plan: Plan) = {
    val (first :: rest) = plan.steps
    new GuideStep(first, Nil, rest)
  }
}

case class InvalidPlanException(error: PlanError) extends RuntimeException(s"Plan is invalid: $error")
