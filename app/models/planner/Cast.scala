package models.planner

import scalaz._
import Scalaz._
import models._
import models.plan._

object Cast {

  def onClosed(from: Needle, until: Needle, withYarn: Yarn): Planner =
    ClosedCastOn(from, until, withYarn)

  def offClosed(withYarn: Yarn, filter: Needle => Boolean = _ => true): Planner =
    ClosedCastOff(withYarn, filter)
}