package connector

import models._

object Connector {
  sealed trait Event
  sealed trait Command

  /** Position of the carriage has changed. */
  case class PositionUpdate(
    position: CarriagePosition,
    direction: Direction,
    carriage: Option[CarriageType])
    extends Event {

    def currentNeedle: Option[Needle] = position match {
      case CarriageOverNeedles(needle) => Some(needle)
      case _ => None
    }
  }

  sealed trait NeedleAction
  case object NeedleToB extends NeedleAction
  case object NeedleToD extends NeedleAction

  /** Load a new pattern row. Must be defined for all needles. */
  case class LoadPatternRow(values: Needle => NeedleAction) extends Command {
    protected def valuesNormalized = Needle.all.map(values).toVector
    override def hashCode = valuesNormalized.hashCode
    override def equals(o: Any) = o match {
      case other: LoadPatternRow => valuesNormalized == other.valuesNormalized
    }
  }
  case class PatternRowLoaded(values: Needle => NeedleAction) extends Event
}