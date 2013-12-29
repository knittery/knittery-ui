package models

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

