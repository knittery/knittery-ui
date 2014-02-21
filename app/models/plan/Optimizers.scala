package models.plan

import scala.util.Try
import scala.annotation.tailrec
import scalaz._
import Scalaz._
import models._
import utils._

object Optimizers {
  val list =
    UnknittedSettingsOptimizer ::
      NoEffectStepOptimizer ::
      OptimizeStepWithNoEffect ::
      OptimizePatternKnitting ::
      OptimizeStepWithNoEffect ::
      Nil
  implicit val all = list.foldLeft(Monoid[PlanOptimizer].zero)(_ |+| _)
  implicit def no = Monoid[PlanOptimizer].zero
}

object OptimizerSupport {
  implicit class RichBooleanFunction[A](val f: A => Boolean) extends AnyVal {
    def or(g: A => Boolean): A => Boolean = a => f(a) || g(a)
    def and(g: A => Boolean): A => Boolean = a => f(a) && g(a)
    def not: A => Boolean = a => !f(a)
  }
}

///** Optimizes away unused ChangeCarriageSetting steps. */
object UnknittedSettingsOptimizer extends PlanOptimizer {
  import OptimizerSupport._
  override def apply(steps: Seq[Step]) = {
    steps.foldRight(List.empty[Step]) {
      case (step: ChangeCarriageSettings, processed) =>
        val necessary = processed.
          takeWhile(changesSettings(step.carriage).not).
          find(knitting(step.carriage)).
          isDefined
        if (necessary) step :: processed
        else processed
      case (step, processed) => step :: processed
    }
  }
  private def changesSettings(c: Carriage) = (s: Step) => s match {
    case s: ChangeCarriageSettings => s.carriage == c
    case AddCarriage(`c`, _) => true
    case _ => false
  }
  private def knitting(c: Carriage) = (s: Step) => s match {
    case KnitRow(carriage, _, _) => c == carriage
    case _ => false
  }
}

/** Optimizes away steps without any effect. */
object NoEffectStepOptimizer extends PlanOptimizer {
  override def apply(steps: Seq[Step]) = {
    steps.foldLeft((Vector.empty[Step], KnittingState.initial)) {
      case ((processed, state), step) =>
        val state2 = step(state).valueOr(e => throw new RuntimeException(e))
        if (state == state2) (processed, state) //optimize it
        else (processed :+ step, state2)
    }._1
  }
}

/** Optimizes away manual MoveNeedles steps. */
object OptimizePatternKnitting extends PlanOptimizer {
  override def apply(steps: Seq[Step]) = {
    val (a, b) = steps.foldLeft((Processed(), Vector.empty[Step])) {
      case ((processed, window), MoveNeedles(bed, pattern)) if processed.state.needles(bed).all == pattern.all =>
        //drop it because needles are already in the required position
        (processed, window)

      case ((processed, (knit @ KnitRow(carriage, direction, _)) +: window), MoveNeedles(bed, pattern)) =>
        //try to optimize
        def patternActions(n: Needle) = if (pattern(n) == NeedleD) NeedleToD else NeedleToB
        val modKnit = KnitRow(carriage, direction, patternActions)
        modKnit(processed.state).map { stateAfter =>
          if (stateAfter.needles(bed).positions.all == pattern.all) {
            //Yay, we optimized all work away, drop the MoveNeedles
            (processed, modKnit +: window)
          } else {
            //Well, there still is some manual work left :(
            ((processed :+ modKnit) ++ window :+ MoveNeedles(bed, pattern), Vector.empty)
          }
        }.leftMap { _ =>
          //Apparently cannot pattern knit with this knitter
          (processed :+ knit :+ MoveNeedles(bed, pattern), Vector.empty)
        }.fold(identity, identity)

      case ((processed, empty), MoveNeedles(bed, pattern)) =>
        //cannot optimize, because nothing is knitted before
        (processed :+ MoveNeedles(bed, pattern), empty)

      case ((processed, window), knit @ KnitRow(KCarriage, _, _)) =>
        (processed ++ window, Vector(knit))
      case ((processed, window), knit @ KnitRow(LCarriage, _, _)) =>
        (processed ++ window, Vector(knit))
      case ((processed, window), OptimizationBoundary(step)) =>
        (processed ++ window :+ step, Vector.empty)
      case ((processed, window), other) =>
        (processed, window :+ other)
    }
    a.steps ++ b
  }
  private case class Processed(steps: Vector[Step] = Vector.empty, state: KnittingState = KnittingState.initial) {
    def :+(step: Step) = {
      val state2 = step(state).valueOr(e => throw new RuntimeException("Plan is not valid: " + e))
      Processed(steps :+ step, state2)
    }
    def ++(add: Traversable[Step]) = add.foldLeft(this)(_ :+ _)
  }
  private object OptimizationBoundary {
    def unapply(s: Step) = s match {
      case step: KnitRow => Some(step)
      case step: ClosedCastOn => Some(step)
      case step: ClosedCastOff => Some(step)
      case _ => None
    }
  }
}

/** Optimizes away steps that have no impact on the result of the knitting. */
object OptimizeStepWithNoEffect extends PlanOptimizer {
  override def apply(steps: Seq[Step]) = {
    def applyState(state: Validation[String, KnittingState], step: Step) = state.flatMap { s =>
      try {
        step.apply(s)
      } catch {
        case e: Exception => e.toString.fail
        case e: NotImplementedError => e.toString.fail
      }
    }
    @tailrec
    def check(checked: List[Step], state: KnittingState, remaining: List[Step]): Seq[Step] = remaining match {
      case current :: tail =>
        val withStep = remaining.foldLeft(state.success[String])(applyState)
        val withoutStep = tail.foldLeft(state.success[String])(applyState)
        if (withStep == withoutStep) {
          // does not change the result, so get rid of it
          check(checked, state, tail)
        } else {
          // keep it, because it changes to output
          val state2 = current(state).valueOr(e => throw new RuntimeException(s"Invalid plan: $e"))
          check(current :: checked, state2, tail)
        }
      case Nil => checked.reverse
    }
    check(Nil, KnittingState.initial, steps.toList)
  }
}