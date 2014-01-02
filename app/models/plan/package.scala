package models

import scalaz._
import Scalaz._

package object plan {
  type NeedleStateRow = Needle => NeedleState
  implicit class RichNeedleStateRow(val row: NeedleStateRow) extends AnyVal {
    def all = Needle.all.map(row)
    def pattern = row.andThen(_.position)
    def yarn = row.andThen(_.yarn)
  }

  type Planner = PlannerM[Unit]
  implicit def planMonoid = Plan.planMonoid
  implicit def plannerMonad = PlannerM.plannerMonad
  implicit def plannerMonoid = Planner.plannerMonoid
  implicit def stepToPlanner(s: KnittingStep) = Planner.step(s)
  implicit def stepToPlannerBindOps(s: KnittingStep) = Planner.stepToPlannerBindOps(s)
  implicit def stepToPlannerFunctorOps(s: KnittingStep) = Planner.stepToPlannerFunctorOps(s)
}