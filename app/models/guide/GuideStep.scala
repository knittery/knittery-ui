package models.guide

import models._
import models.plan._

class GuideStep private (val step: Step, private val processedReversed: List[Step], private val remaining: List[Step]) {
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

  def name = step match {
    case ClosedCastOn(_, _, _) => "Cast on"
    case ClosedCastOff(_, _) => "Cast off"
    case AddCarriage(_, _) => "Add Carriage"
    case ThreadYarn(_, _) => "Thread Yarn"
    case KnitRow(_, _, _) => "Knit Row"
    case ChangeCarriageSettings(_) => "Change Settings"
  }

  def stateAfter: KnittingState = {
    step(stateBefore).valueOr(e => throw InvalidPlanException(PlanError(step, stepNumber, e)))
  }
  def stateBefore: KnittingState = {
    if (isFirst) KnittingState.initial
    else previous.stateAfter
  }

  /** List of all steps. */
  def all = first.allFromHere
  /** List of all steps that come after this step (including this). */
  def allFromHere: List[GuideStep] =
    this :: (if (isLast) Nil else next.allFromHere)
  /** List of all steps that come before this step (including this). */
  def allUpToHere: Seq[GuideStep] =
    if (isFirst) Vector(this) else previous.allUpToHere :+ this

  override def toString = s"GuideStep($step)"
  override def hashCode = step.hashCode ^ remaining.hashCode
  override def equals(o: Any) = o match {
    case other: GuideStep =>
      other.step == step && other.processedReversed == processedReversed &&
        other.remaining == remaining
    case _ => false
  }
}

object GuideStep {
  def apply(plan: Plan) = {
    val (first :: rest) = plan.steps
    new GuideStep(first, Nil, rest)
  }
}

case class InvalidPlanException(error: PlanError) extends RuntimeException(s"Plan is invalid: $error")
