package models.plan

import scala.util.Try
import scalaz._
import scalaz.syntax._
import scalaz.Scalaz._
import utils._

/** Monad to create a KnittingPlan. */
sealed trait PlannerM[+A] {
  def plan(optimizer: PlanOptimizer = Optimizers.all) = {
    val t = System.currentTimeMillis
    println("Starting to create plan")
    val p = buildPlan(KnittingState.initial).map {
      case (_, plan) =>
        println(s"got basic plan in ${System.currentTimeMillis - t} ms. Plan has ${plan.steps.size} steps.")
        val cachedPlan = Plan(optimizer(plan.steps)).cache(KnittingState.initial)
        cachedPlan.run
        println("Called plan.run")
        cachedPlan.run
        println("Called plan.run again")
        cachedPlan
    }
    println(s"Plan created&optimized in ${System.currentTimeMillis - t} ms")
    p
  }

  protected[PlannerM] def buildPlan(state: KnittingState): Validation[String, (A, Plan)]
}
object PlannerM {
  type Planner = PlannerM[Unit]

  def step(step: Step): Planner = new Planner {
    override def buildPlan(state: KnittingState) = ((), Plan(step)).success
  }
  def validate[A](f: KnittingState => Validation[String, A]): PlannerM[A] = new PlannerM[A] {
    override def buildPlan(state: KnittingState) = f(state).map((_, Monoid[Plan].zero))
  }

  def plannerMonad: Monad[PlannerM] = new Monad[PlannerM] {
    override def point[A](a: => A) = new PlannerM[A] {
      override def buildPlan(state: KnittingState) = (a, Monoid[Plan].zero).success
    }
    override def bind[A, B](pa: PlannerM[A])(fb: A => PlannerM[B]) = {
      new PlannerM[B] {
        override def buildPlan(state: KnittingState) = for {
          (a, planA) <- pa.buildPlan(state)
          cachedPlanA = planA.cache(state) // Performance improvement
          stateAfterA <- cachedPlanA.run(state).leftMap(_.toString)
          pb = fb(a)
          (b, planB) <- pb.buildPlan(stateAfterA)
        } yield (b, cachedPlanA |+| planB)
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