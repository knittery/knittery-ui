package models.planner

import scala.util.Try
import scalaz._
import Scalaz._
import models._
import models.plan._
import utils._

trait Planner {
  def plan = partial(KnittingState.initial)
  protected def partial(state: KnittingState): Validation[String, KnittingPlan]

  def +(other: Planner): Planner = this append (_ => other)
  def append(f: KnittingState => Planner): Planner = {
    val me = this
    new Planner {
      override def partial(state: KnittingState) = {
        for {
          part1 <- me.partial(state)
          state1 <- part1.run(state).leftMap(_.toString)
          part2 <- f(state1).partial(state1)
        } yield part1 |+| part2
      }
    }
  }
}

object Planner {
  implicit def apply(f: KnittingState => Validation[String, KnittingPlan]): Planner = new Planner {
    override def partial(state: KnittingState) = f(state)
  }

  /** Planner from steps. */
  def steps(f: KnittingState => Validation[String, Seq[KnittingStep]]): Planner =
    Planner(f.andThen(_.map(KnittingPlan)))

  /** Planner from optional step. */
  def optStep(f: KnittingState => Validation[String, Option[KnittingStep]]): Planner =
    Planner.steps(f.andThen(_.map(_.toList)))

  /** Single step planner. */
  def step(f: KnittingState => Validation[String, KnittingStep]): Planner =
    Planner.steps(f.andThen(_.map(List(_))))

  /** Planner that only does validation (no step). */
  def validate(f: KnittingState => Validation[String, Unit]): Planner =
    Planner.steps(f.andThen(_.map(_ => Nil)))

  /** Planner that only does validation (no step). Catches exceptions. */
  def validateTry(f: KnittingState => Unit): Planner = Planner.steps { state =>
    Try { f(state) }.toSuccess.map(_ => Nil)
  }

  def empty = steps(_ => Nil.success)
}