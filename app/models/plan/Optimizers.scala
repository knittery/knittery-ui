package models.plan

import scala.annotation.tailrec
import scalaz._
import Scalaz._
import models._

object Optimizers {
  val list =
    UnknittedSettingsOptimizer ::
      NoEffectStepOptimizer ::
      OptimizeUselessMoveNeedles ::
      OptimizePatternKnitting ::
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

/** Optimizes away steps without an effect. */
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

/** Optimizes away a manual MoveNeedles steps if B or D does not matter to the next knitting. */
object OptimizeUselessMoveNeedles extends PlanOptimizer {
  override def apply(steps: Seq[Step]) = {
    steps.
      foldLeft((Vector.empty[(Step, KnittingState)], KnittingState.initial)) {
        case ((out, state), step) =>
          val state2 = step(state).valueOr(e => throw new RuntimeException("Plan is not valid: " + e))
          (out :+ (step, state2), state2)
      }._1.foldRight(List.empty[(Step, KnittingState)]) {
        case ((step @ MoveNeedles(bed, to), stateBefore), processed) =>
          val posBefore = stateBefore.needles(bed).positions
          val onlyBToD = Needle.all.filter(n => (posBefore(n), to(n)) match {
            case (a, b) if a == b => false
            case (NeedleB, NeedleD) => false
            case (NeedleD, NeedleB) => false
            case _ => true
          }).isEmpty
          if (onlyBToD) {
            processed.find {
              case (k: KnitRow, state) => true
              case _ => false
            } match { //next knitting
              case Some((KnitRow(KCarriage, dir, _), state)) =>
                val s = state.carriageState(KCarriage).settings
                val bOrDimportant = s.mc || s.l ||
                  (dir == ToLeft && (s.partLeft || s.tuckLeft)) ||
                  (dir == ToRight && (s.partRight || s.tuckRight))
                if (bOrDimportant) (step, stateBefore) :: processed
                else processed //got rid of it
              case Some(_) =>
                //cannot optimize it away
                (step, stateBefore) :: processed
              case None =>
                //drop it because it's not used for knitting
                processed
            }
          } else (step, stateBefore) :: processed
        case (x, processed) =>
          x :: processed
      }.map(_._1)
  }
}