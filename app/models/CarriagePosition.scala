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
  def towards: LeftRight
}
case object ToLeft extends Direction {
  override def reverse = ToRight
  def towards = Left
}
case object ToRight extends Direction {
  override def reverse = ToLeft
  def towards = Right
}

/** Left or right. */
sealed trait LeftRight {
  def reverse: LeftRight
  def direction: Direction
}
case object Left extends LeftRight {
  override def reverse = Right
  def direction = ToLeft
}
case object Right extends LeftRight {
  override def reverse = Left
  def direction = ToRight
}