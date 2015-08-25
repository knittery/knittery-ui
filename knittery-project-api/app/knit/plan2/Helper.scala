package knit.plan2

import scalaz._
import Scalaz._
import knit._
import knit.plan2.KnittingPlan._

object Helper {
  /** Working needles (across all bed) */
  def workingNeedles: KnittingPlan[Set[Needle]] =
    Beds.all.foldMap(workingNeedles)

  /** Working needles on the specified bed. */
  def workingNeedles(bed: Bed): KnittingPlan[Set[Needle]] = for {
    positions <- needlePositions(bed)
    a = Needle.all.filter(n => positions(n).isWorking).toSet
  } yield a


  /** Next direction to use for the carriage. Will fail if the carriage is not on the board. */
  def nextDirection(carriage: Carriage): KnittingPlan[Direction] = (for {
    pos <- carriagePosition(carriage)
    working <- workingNeedles
  } yield pos match {
      case CarriageRemoved => s"Trying to use removed carriage ${carriage.name}.".left
      case pos =>
        if (working.intersect(carriage.over(pos).toSet).nonEmpty)
          s"carriage ${carriage.name} still over working needeles".left
        else pos.directionTo(working.headOption.getOrElse(Needle.middle)).right
    }).flatten

}
