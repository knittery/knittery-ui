package models.plan

import scala.util.Try
import scalaz._
import scalaz.syntax._
import scalaz.Scalaz._
import utils._

/** Monad to create a KnittingPlan. */
sealed trait PlannerM[+A] {
  def plan = run(KnittingState.initial).map(_._2)

  protected[PlannerM] def run(state: KnittingState): Validation[String, (A, KnittingPlan)]
}
object PlannerM {
  type Planner = PlannerM[Unit]

  def step(step: KnittingStep): Planner = new Planner {
    override def run(state: KnittingState) = ((), KnittingPlan(step :: Nil)).success
  }
  def validate[A](f: KnittingState => Validation[String, A]) = new PlannerM[A] {
    override def run(state: KnittingState) = f(state).map((_, Monoid[KnittingPlan].zero))
  }

  def plannerMonad: Monad[PlannerM] = new Monad[PlannerM] {
    override def point[A](a: => A) = new PlannerM[A] {
      override def run(state: KnittingState) = (a, Monoid[KnittingPlan].zero).success
    }
    override def bind[A, B](pa: PlannerM[A])(fb: A => PlannerM[B]) = {
      new PlannerM[B] {
        override def run(state: KnittingState) = for {
          (a, planA) <- pa.run(state)
          stateAfterA <- planA.run(state).leftMap(_.toString)
          pb = fb(a)
          (b, planB) <- pb.run(stateAfterA)
        } yield (b, planA |+| planB)
      }
    }
  }
}

object Planner {
  /** Add a step to the PlannerM. */
  implicit def step(step: KnittingStep): Planner = PlannerM.step(step)

  /** Read a value from the state. */
  def state[A](f: KnittingState => A) = PlannerM.validate(f(_).success)

  /** Read a value from the state. */
  def validate[A](f: KnittingState => Validation[String, A]) = PlannerM.validate(f)

  /** PlannerM that only does validation (no step). Catches exceptions. */
  def precondidtions[A](f: KnittingState => A): PlannerM[A] = validate(s => Try(f(s)).toSuccess)

  /** Alias for zero. */
  def noop = Monoid[Planner].zero

  /** Planner is a monoid. */
  def plannerMonoid: Monoid[Planner] = new Monoid[Planner] {
    override def zero = Monad[PlannerM].point(())
    override def append(a: Planner, b: => Planner) = a >> b
  }
  //Allows to use steps directly as plans in a for comprehension
  def stepToPlannerBindOps(s: KnittingStep): BindOps[PlannerM, Unit] = step(s)
  def stepToPlannerFunctorOps(s: KnittingStep): FunctorOps[PlannerM, Unit] = step(s)
}