package knit.plan2

import scalaz._
import Scalaz._
import knit._
import knit.plan2.KnittingPlan._

object Helper {
  def noop = value(())

  /** Require that the condition is true, else fail knitting with the error. */
  def require(condition: Boolean, error: String = "requirement failed") = {
    if (condition) value(())
    else knittingError(error)
  }

  /** Working needles (across all bed) */
  def workingNeedles: KnittingPlan[Set[Needle]] =
    Beds.all.foldMap(workingNeedles)

  /** Working needles on the specified bed. */
  def workingNeedles(bed: Bed): KnittingPlan[Set[Needle]] = for {
    positions <- needlePositions(bed)
    a = Needle.all.filter(n => positions(n).isWorking).toSet
  } yield a

  //
  /** Add carriage if missing. */
  def needCarriage(carriage: Carriage, addAt: LeftRight = Left): KnittingPlan[Unit] = {
    carriagePosition(carriage).map(_.onBoard).ifM(
      noop,
      addCarriage(carriage, addAt)
    )
  }

  /** Change K settings and make sure the carriage is on the board. */
  def carriageSettings(settings: KCarriage.Settings, assembly: KCarriage.Assembly) = {
    needCarriage(KCarriage) >> changeKCarriageSettings(settings, assembly)
  }
  /** Change L settings and make sure the carriage is on the board. */
  def carriageSettings(settings: LCarriage.Settings) = {
    needCarriage(LCarriage) >> changeLCarriageSettings(settings)
  }
  /** Change G settings and make sure the carriage is on the board. */
  def carriageSettings(settings: GCarriage.Settings) = {
    needCarriage(GCarriage) >> changeGCarriageSettings(settings)
  }

  /** Next direction to use for the carriage. Will fail if the carriage is not on the board. */
  def nextDirection(carriage: Carriage): KnittingPlan[Direction] = (for {
    pos <- carriagePosition(carriage)
    working <- workingNeedles
  } yield {
      if (pos.onBoard)
        if (working.intersect(carriage.over(pos).toSet).nonEmpty)
          s"carriage ${carriage.name} still over working needeles".left
        else pos.directionTo(working.headOption.getOrElse(Needle.middle)).right
      else s"Trying to use removed carriage ${carriage.name}.".left
    }).flatten

}
