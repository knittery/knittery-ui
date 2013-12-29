package models.planner

import scalaz._
import Scalaz._
import models._
import models.plan._
import PlannerUtils._

object CastOnPlanner {

  def closed(from: Needle, until: Needle, withYarn: Yarn): Planner = { initial =>
    StepAcc(ClosedCastOn(from, until, withYarn), initial).toPlan
  }

}