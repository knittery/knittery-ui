package models

import scalaz._
import Scalaz._
import scalaz.syntax.BindOps
import models.plan._

package object planner {
  type Planner = PlannerM[Unit]

  implicit def plannerMonad = PlannerM.plannerMonad
  implicit def plannerMonoid = Planner.plannerMonoid
  implicit def stepToPlanner(s: KnittingStep) = Planner.step(s)
  implicit def stepToPlannerBindOps(s: KnittingStep) = Planner.stepToPlannerBindOps(s)
  implicit def stepToPlannerFunctorOps(s: KnittingStep) = Planner.stepToPlannerFunctorOps(s)
}