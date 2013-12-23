package models

/** Knitting position of a needle. */
sealed trait NeedlePosition
/** Needle in A position. */
case object NeedleA extends NeedlePosition
/** Needle in B position. */
case object NeedleB extends NeedlePosition
/** Needle in C position. */
case object NeedleC extends NeedlePosition
/** Needle in D position. */
case object NeedleD extends NeedlePosition

/**
 * Positions of needles.
 */
trait NeedlePattern {
  /** Number of rows in the pattern. */
  def height: Int

  /** Value at the position (pattern starts at 0). */
  def apply(row: Int)(needle: Needle): NeedlePosition
}

object NeedlePattern {
  val empty = new NeedlePattern {
    def height = 1
    def apply(row: Int)(needle: Needle) = NeedleA
  }
}