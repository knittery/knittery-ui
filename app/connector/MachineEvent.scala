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

sealed trait CarriagePosition
case class CarriageOverNeedles(
  /** Needle the middle of the carriage is over. */
  currentNeedle: Needle)
  extends CarriagePosition
case class CarriageLeft(
  /** 0 if carriage left the board completely. */
  overlappedNeedles: Int)
  extends CarriagePosition
case class CarriageRight(
  /** 0 if carriage left the board completely. */
  overlappedNeedles: Int)
  extends CarriagePosition

sealed trait Direction
case object Left extends Direction
case object Right extends Direction

sealed trait CarriageType
/** Normal carriage. */
case object KCarriage extends CarriageType
case object LCarriage extends CarriageType
/** Electronic carriage. */
case object GCarriage extends CarriageType