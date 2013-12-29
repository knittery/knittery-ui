package models.planner

import scalaz._
import Scalaz._
import models._
import models.plan._

object CastOnPlanner {

  def closed(from: Needle, until: Needle, withYarn: Yarn) = Planner.step { _ =>
    ClosedCastOn(from, until, withYarn).success
  }

}