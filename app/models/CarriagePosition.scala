package models

sealed trait CarriagePosition {
  def directionTo(needle: Needle): Direction
}

case class CarriageOverNeedles(
  /** Needle the middle of the carriage is over. */
  currentNeedle: Needle)
  extends CarriagePosition {

  override def directionTo(needle: Needle) = if (needle < currentNeedle) Left else Right
}

case class CarriageLeft(
  /** 0 if carriage left the board completely. */
  overlappedNeedles: Int)
  extends CarriagePosition {

  override def directionTo(needle: Needle) = Right
}

case class CarriageRight(
  /** 0 if carriage left the board completely. */
  overlappedNeedles: Int)
  extends CarriagePosition {

  override def directionTo(needle: Needle) = Left
}

/** Direction of movement */
sealed trait Direction {
  def reverse: Direction
}
case object Left extends Direction {
  override def reverse = Right
}
case object Right extends Direction {
  override def reverse = Left
}

