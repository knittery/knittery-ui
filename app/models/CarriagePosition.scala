package models

sealed trait CarriagePosition {
  def directionTo(needle: Needle): Direction
}

case class CarriageOverNeedles(
  /** Needle the middle of the carriage is over. */
  currentNeedle: Needle)
  extends CarriagePosition {

  override def directionTo(needle: Needle) = if (needle < currentNeedle) ToLeft else ToRight
}

case class CarriageLeft(
  /** 0 if carriage left the board completely. */
  overlappedNeedles: Int)
  extends CarriagePosition {

  override def directionTo(needle: Needle) = ToRight
}

case class CarriageRight(
  /** 0 if carriage left the board completely. */
  overlappedNeedles: Int)
  extends CarriagePosition {

  override def directionTo(needle: Needle) = ToLeft
}

/** The carriage is not in use. */
case object CarriageRemoved extends CarriagePosition {
  override def directionTo(needle: Needle) =
    throw new IllegalStateException(s"Removed carriage has no direction to $needle")
}

/** Direction of movement */
sealed trait Direction {
  def reverse: Direction
}
case object ToLeft extends Direction {
  override def reverse = ToRight
}
case object ToRight extends Direction {
  override def reverse = ToLeft
}

/** Left or right. */
sealed trait LeftRight {
  def reverse: LeftRight
}
case object Left extends LeftRight {
  override def reverse = Right
}
case object Right extends LeftRight {
  override def reverse = Left
}