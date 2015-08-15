package knit

sealed trait Needle extends Ordered[Needle] {
  /** Numbered from left to right starting with 0 (leftmost needle is 0). */
  def index: Int
  /** Number as shown on the machine (-100 to 100, 0 does not exist, negative are yellow). */
  def number: Int
  /** Format: +3 or -4 */
  def numberString = if (number > 0) s"+$number" else number.toString

  def +(i: Int) = Needle.atIndex(index + i)
  def -(i: Int) = this + (-i)
  def distanceTo(other: Needle) = (index - other.index).abs

  def safeAdd(i: Int) = Needle.atIndex((index + i) min (Needle.count - 1) max 0)
  def safeSubtract(i: Int) = safeAdd(-i)
}

object Needle {
  val count = 200
  val all = (0 until count).map(atIndex)
  val middle = atIndex(count / 2)

  def interval(from: Needle, to: Needle) = {
    if (from < to) (from.index to to.index).map(atIndex)
    else (from.index to to.index by -1).map(atIndex)
  }

  def atIndex(index: Int): Needle = {
    require(index >= 0)
    require(index < count)
    NeedleImpl(index)
  }

  private case class NeedleImpl(index: Int) extends Needle {
    override val number = {
      if (index < 100) -100 + index
      else index - 99
    }
    override def compare(other: Needle) = index - other.index
    override def toString = s"Needle($index)"
  }
}

/** Knitting position of a needle. */
sealed trait NeedlePosition {
  def isWorking: Boolean
  def nonWorking = !isWorking
}
/** Needle in A position. */
case object NeedleA extends NeedlePosition {
  override def isWorking = false
  override def toString = "A"
}
/** Needle in B position. */
case object NeedleB extends NeedlePosition {
  override def isWorking = true
  override def toString = "B"
}
/** Needle in D position. */
case object NeedleD extends NeedlePosition {
  override def isWorking = true
  override def toString = "D"
}
/** Needle in E position. */
case object NeedleE extends NeedlePosition {
  override def isWorking = true
  override def toString = "E"
}

/** Action to perform with a needle (by the carriage). */
sealed trait NeedleAction {
  def toPosition: NeedlePosition
}
/** Needle is moved to B position. */
case object NeedleToB extends NeedleAction {
  override def toPosition = NeedleB
}
/** Needle is moved to D position. */
case object NeedleToD extends NeedleAction {
  override def toPosition = NeedleD
}


