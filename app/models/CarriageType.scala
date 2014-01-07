package models

sealed trait CarriageType {
  def name: String
  /** Needles the carriage is over if at the given position. */
  def over(pos: CarriagePosition): Seq[Needle] = pos match {
    case CarriageLeft(overlapCount) => Needle.all.take(overlapCount)
    case CarriageRight(overlapCount) => Needle.all.reverse.take(overlapCount)
    case CarriageOverNeedles(needle) => Needle.all.span(_ == needle) match {
      case (a, b) => a.reverse.take(width / 2).reverse ++ b.take(width / 2)
    }
  }
  /** Width of the carriage in needles. */
  protected def width: Int
}

/** Normal carriage. */
case object KCarriage extends CarriageType {
  override def name = "K"
  override protected def width = 66
}

/** Lace pattern carriage. */
case object LCarriage extends CarriageType {
  override def name = "L"
  override protected def width = 46
}

/** Electronic carriage. */
case object GCarriage extends CarriageType {
  override def name = "G"
  override protected def width = 20
}
