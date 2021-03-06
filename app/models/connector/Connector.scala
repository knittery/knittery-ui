package models.connector

import models._

object Connector {
  sealed trait Event
  sealed trait Command

  /** Position of the carriage has changed. */
  case class PositionUpdate(
    position: CarriagePosition,
    direction: Direction,
    carriage: Option[Carriage])
    extends Event {

    def currentNeedle: Option[Needle] = position match {
      case CarriageOverNeedles(needle) => Some(needle)
      case _ => None
    }
  }

  /** Load a new pattern row. Must be defined for all needles. */
  case class LoadPatternRow(values: Needle => NeedleAction) extends Command {
    protected def valuesNormalized = Needle.all.map(values).toVector
    override def hashCode = valuesNormalized.hashCode
    override def equals(o: Any) = o match {
      case other: LoadPatternRow => valuesNormalized == other.valuesNormalized
    }
  }
  case class PatternRowLoaded(values: Needle => NeedleAction) extends Event
  case object PatternRowLoadFailure extends Event
}