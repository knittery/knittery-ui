package models.guide

import models._
import models.plan._

sealed trait GuideStep {
  val step: Step
  protected def remaining: List[Step]

  lazy val next = {
    require(!isLast, "Already at end")
    val prev = this
    val (n :: rest) = remaining
    new GuideStep {
      override def previous = prev
      override val step = n
      override val remaining = rest
    }
  }
  def previous: GuideStep
  def first: GuideStep = if (isFirst) this else previous.first
  def isFirst = false
  def last: GuideStep = if (isLast) this else next.last
  def isLast = remaining.isEmpty

  /** Number of this step (1-based). */
  def stepNumber: Int = previous.stepNumber + 1

  def name = info._1
  def description = info._2

  def isKnitting = step match {
    case KnitRow(_, _, _) => true
    case other => false
  }

  /** Needles that have to be manually processed. */
  def manualNeedles: Set[Needle] = step match {
    case c @ ClosedCastOn(_, _, _) =>
      c.needles.toSet
    case ClosedCastOff(_, f) =>
      Needle.all.filter(f).toSet
    case MoveNeedles(to) =>
      Needle.all.filter(n => stateBefore.needles(n).position != to(n)).toSet
    case other => Set.empty
  }

  private def info = step match {
    case ClosedCastOn(from, to, yarn) =>
      ("Cast on",
        s"Perform a closed cast on from needle ${from.number} until ${to.number} with yarn ${yarn.yarn.name}")
    case ClosedCastOff(yarn, filter) =>
      val from = Needle.all.filter(filter).head
      val to = Needle.all.filter(filter).last
      ("Cast off",
        s"Perform a closed cast off for needles ${from.number} through ${to.number} with ${yarn.yarn.name}")

    case ThreadYarnK(None, None) =>
      (s"Unthread Yarn from K",
        s"Unthread all yarns from the K carriage")
    case ThreadYarnK(Some(yarn), None) =>
      (s"Thread Yarn ${yarn.yarn.name} to K",
        s"Thread the yarn ${yarn.yarn.name} into A on the K carriage")
    case ThreadYarnK(None, Some(yarn)) =>
      (s"Thread Yarn ${yarn.yarn.name} to K",
        s"Thread the yarn ${yarn.yarn.name} into B on the K carriage")
    case ThreadYarnK(Some(yarnA), Some(yarnB)) =>
      (s"Thread Yarns ${yarnA.yarn.name} and ${yarnB.yarn.name} to K",
        s"Thread the yarns ${yarnA.yarn.name} into A and ${yarnB.yarn.name} into B on the K carriage")
    case ThreadYarnG(None) =>
      (s"Unthread Yarn from G",
        s"Unthread the yarn from the G carriage")
    case ThreadYarnG(Some(yarn)) =>
      (s"Thread Yarn ${yarn.yarn.name} to G",
        s"Thread the yarn ${yarn.yarn.name} to the G carriage")

    case MoveNeedles(to) =>
      val affected = Needle.all.filter(n => stateBefore.needles(n).position != to(n))
      val movement = affected.map(n => s"${n.number} to ${to(n).toString}")
      (s"Move ${affected.size} needles by hand",
        s"Move to following needles: ${movement.mkString(", ")}")

    case KnitRow(c, dir, _) =>
      val from = if (ToLeft != dir) "left" else "right"
      val to = if (ToLeft == dir) "left" else "right"
      (s"Knit Row (${c.name})",
        s"Knit a row from $from to $to with the ${c.name} carriage")

    case AddCarriage(c, at) =>
      val lr = if (ToLeft == at) "left" else "right"
      (s"Add Carriage ${c.name}",
        s"Add the carriage ${c.name} at the $lr side")

    case ChangeKCarriageSettings(s) =>
      def button(name: String, value: Boolean) =
        name + "=" + (if (value) "on" else "off")
      val settings = button("mc", s.mc) ::
        button("L", s.l) ::
        button("left part", s.partLeft) ::
        button("right part", s.partRight) ::
        button("left tuck", s.tuckLeft) ::
        button("right tuck", s.tuckRight) ::
        Nil
      (s"Change Settings K",
        s"Change K carriage settings Knob to ${s.knob} with lever at ${s.holdingCamLever.name} and ${settings.mkString(" and ")}")

    case Information(title, desc) => (title, desc)
  }

  lazy val stateAfter: KnittingState = {
    step(stateBefore).valueOr(e => throw InvalidPlanException(PlanError(step, stepNumber, e)))
  }
  def stateBefore: KnittingState = previous.stateAfter

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
      other.step == step && (isFirst || other.previous == previous) &&
        other.remaining == remaining
    case _ => false
  }
}

object GuideStep {
  def apply(plan: Plan): GuideStep = {
    val (fst :: rest) = plan.steps.toList
    new GuideStep {
      override val step = fst
      override val remaining = rest :+ finalStep
      override def previous = throw new IllegalArgumentException("already at start")
      override def isFirst = true
      override def stepNumber = 1
      override def stateBefore = KnittingState.initial
    }
  }
  private val finalStep = Information("Done", "Knitting is done")
}

case class InvalidPlanException(error: PlanError) extends RuntimeException(s"Plan is invalid: $error")
