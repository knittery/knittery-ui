package models.plan

import scala.annotation.tailrec
import scalaz._
import Scalaz._
import models._

object Optimizers {
  val list = UnknittedSettingsOptimizer ::
    DuplicateSettingsOptimizer ::
    Nil
  implicit val all = list.foldLeft(Monoid[PlanOptimizer].zero)(_ |+| _)
  implicit def no = Monoid[PlanOptimizer].zero
}

object OptimizerSupport {
  def matches(p: PartialFunction[Step, Any]) = p.isDefinedAt _
  implicit class RichBooleanFunction[A](val f: A => Boolean) extends AnyVal {
    def or(g: A => Boolean): A => Boolean = a => f(a) || g(a)
    def and(g: A => Boolean): A => Boolean = a => f(a) && g(a)
    def not: A => Boolean = a => !f(a)
  }

  def isChangeSettings = matches { case ChangeCarriageSettings(_) => () }
  def isChangeSettings(settings: CarriageSettings) = matches {
    case ChangeCarriageSettings(s) if s.carriage == settings.carriage => ()
  }
  def isKnitting(settings: CarriageSettings) = matches {
    case KnitRow(settings.carriage, _, _) => ()
  }
}

/** Optimizes away unused ChangeCarriageSetting steps. */
object UnknittedSettingsOptimizer extends PlanOptimizer {
  override def apply(steps: Seq[Step]) = {
    import OptimizerSupport._
    @tailrec
    def exec(steps: Seq[Step], optimized: Seq[Step]): Seq[Step] = steps.span(isChangeSettings.not) match {
      case (before, (step @ ChangeCarriageSettings(current)) +: after) =>
        def processed = optimized ++ before
        after.span(isKnitting(current).not and isChangeSettings(current).not) match {
          case (between, ChangeCarriageSettings(settings) +: rest) =>
            // Settings changed before knitting is performed, drop it
            exec(after, processed)
          case (between, KnitRow(_, _, _) +: rest) =>
            // Knitting performed with this settings, so we need it
            exec(after, processed :+ step)
          case _ =>
            // no knitting performed afterwards, drop it
            exec(after, processed)
        }
      case noMoreChangeSettings =>
        optimized ++ steps
    }
    exec(steps, Vector.empty)
  }
}

/** Optimizes away duplicate ChangeCarriageSetting steps. */
object DuplicateSettingsOptimizer extends PlanOptimizer {
  override def apply(steps: Seq[Step]) = {
    steps.foldLeft((Vector.empty[Step], Map.empty[CarriageType, CarriageSettings])) {
      case (v @ (_, settings), ChangeCarriageSettings(s)) if settings.get(s.carriage) == Some(s) =>
        //drop it, it sets to the same settings
        v
      case ((processed, settings), step @ ChangeCarriageSettings(s)) =>
        //new setting
        (processed :+ step, settings + (s.carriage -> s))
      case ((processed, settings), step) => (processed :+ step, settings)
    }._1
  }
}