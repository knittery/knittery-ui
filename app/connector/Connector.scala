package connector

import models._

object Connector {
  sealed trait Event

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
}