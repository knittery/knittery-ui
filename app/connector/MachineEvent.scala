package connector

import models._

sealed trait MachineEvent

/** Position of the carriage has changed. */
case class PositionUpdate(
  position: CarriagePosition,
  direction: Direction,
  carriage: Option[CarriageType])
  extends MachineEvent {

  def currentNeedle: Option[Needle] = position match {
    case CarriageOverNeedles(needle) => Some(needle)
    case _ => None
  }
}