package models.planner

import scalaz._
import Scalaz._
import models._
import models.plan._

trait Planner {
  def plan = partial(KnittingState.initial)

  protected def partial(state: KnittingState): Validation[String, KnittingPlan]

  def +(other: Planner): Planner = {
    val me = this
    new Planner {
      override def partial(state: KnittingState) = {
        for {
          part1 <- me.partial(state)
          state1 <- part1.run(state).leftMap(_.toString)
          part2 <- other.partial(state1)
        } yield part1 + part2
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
    Planner(f.andThen(_.map(KnittingPlan(_))))

  /** Single step planner. */
  def step(f: KnittingState => Validation[String, KnittingStep]): Planner =
    Planner.steps(f.andThen(_.map(List(_))))

  def empty = steps(_ => Nil.success)
}