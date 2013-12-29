package models

import scalaz._
import Scalaz._
import models.plan._

package object planner {
  type Planner = KnittingState => Validation[String, KnittingPlan]

  object PlannerUtils {
    type StepAcc = Validation[String, (KnittingState, List[KnittingStep])]

    implicit class RichStepAcc(val acc: StepAcc) extends AnyVal {
      def toPlan = acc.map(_._2.reverse).map(KnittingPlan(_))
    }
    object StepAcc {
      def apply(step0: KnittingStep, initialState: KnittingState): StepAcc =
        step0(initialState).map(s2 => (s2, List(step0)))
    }

    implicit class StepAccRunner[A](val t: Traversable[A]) {
      /**
       * Make a step acc from the Traversable.
       * Every iteration returns (element processed [will be fed to nextStep again if false], produced step).
       */
      def foldStep(initial: StepAcc)(nextStep: (KnittingState, A) => Validation[String, (Boolean, KnittingStep)]): StepAcc = {
        def perform(elem: A)(state: KnittingState, steps: List[KnittingStep]): StepAcc = {
          nextStep(state, elem).flatMap {
            case (processed, step) => step(state).map((processed, step, _))
          } match {
            case Success((true, step, s2)) => (s2, step :: steps).success
            case Success((false, step, s2)) => perform(elem)(s2, step :: steps)
            case Failure(msg) => msg.fail
          }
        }
        t.foldLeft(initial)((x, elem) => x.flatMap((perform(elem) _).tupled))
      }
    }
  }
}