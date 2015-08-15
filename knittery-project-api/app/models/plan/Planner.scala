package models.plan

import scala.util.Try
import scalaz._
import scalaz.syntax._
import scalaz.Scalaz._
import utils._

/** Monad to create a KnittingPlan. */
sealed trait PlannerM[+A] {
  def plan(optimizer: PlanOptimizer = Optimizers.all) = {
    buildPlan(StartPlan).map {
      case (_, plan) => optimizer(plan)
    }
  }

  protected[PlannerM] def buildPlan(before: Plan): Validation[String, (A, Plan)]
}
object PlannerM {
  type Planner = PlannerM[Unit]

  def step(step: Step): Planner = new Planner {
    override def buildPlan(before: Plan) = CompositePlan(before, step).map(p => ((), p)).leftMap(_.toString)
  }
  def validate[A](f: KnittingState => Validation[String, A]): PlannerM[A] = new PlannerM[A] {
    override def buildPlan(before: Plan) = f(before.run).map((_, before))
  }

  def plannerMonad: Monad[PlannerM] = new Monad[PlannerM] {
    override def point[A](a: => A) = new PlannerM[A] {
      override def buildPlan(before: Plan) = (a, before).success
    }
    override def bind[A, B](pa: PlannerM[A])(fb: A => PlannerM[B]) = {
      new PlannerM[B] {
        override def buildPlan(before: Plan) = for {
          (a, planA) <- pa.buildPlan(before)
          pb = fb(a)
          (b, planB) <- pb.buildPlan(planA)
        } yield (b, planB)
      }
    }
  }
}

object Planner {
  /** Add a step to the PlannerM. */
  implicit def step(step: Step): Planner = PlannerM.step(step)

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
  def stepToPlannerBindOps(s: Step): BindOps[PlannerM, Unit] = step(s)
  def stepToPlannerFunctorOps(s: Step): FunctorOps[PlannerM, Unit] = step(s)
}