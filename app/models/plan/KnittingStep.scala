package models.plan

import scala.util.Try
import models._

/** Step to perform during knitting. */
sealed trait KnittingStep extends Function[KnittingState, Try[KnittingState]] {
  /** Check if the step can be applied to the state. */
  def checkPreconditions(state: KnittingState) = apply(state).isSuccess

  protected def invalidState(reason: String) = throw new IllegalArgumentException(reason)
}

trait KnitARow extends KnittingStep {
  def carriage: CarriageType
  def direction: Direction

  def yarnA: Option[Yarn]
  def yarnB: Option[Yarn]
}

case class KnitPatternRow(carriage: CarriageType, direction: Direction, patternToSet: NeedlePatternRow,
  yarnA: Option[Yarn], yarnB: Option[Yarn] = None) extends KnitARow {

  override def apply(state: KnittingState) = Try {
    val (settings, pos) = state.carriages.get(carriage).getOrElse(invalidState(s"Undefined state for $carriage"))
    //Preconditions
    (pos, direction) match {
      case (CarriageLeft(_), Left) => invalidState("Cannot move carriage from left to left")
      case (CarriageRight(_), Right) => invalidState("Cannot move carriage from right to right")
      case (_, _) => () // ok
    }
    val needlesBeforePattern = settings.applyOnNeedles(direction)(state.needles)
    Needle.all.foreach { n =>
      (needlesBeforePattern(n), patternToSet(n)) match {
        case (NeedleB, NeedleD) => () //pattern knitting does that
        case (NeedleD, NeedleB) => () //pattern knitting does that
        case (a, b) if a == b => ()
        case (a, b) => invalidState(s"Cannot set needle ${n.number} from $a to $b")
      }
    }
    
    state
  }

}

trait ChangeCarriageSettings extends KnittingStep {
  def carriage: CarriageType
}

trait ManualAction extends KnittingStep {
  def name: String

  /** Description of the action to perform. May use basic HTML syntax. */
  def description: String
}
