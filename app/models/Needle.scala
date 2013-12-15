package models

sealed trait Needle {
  /** Numbered from left to right starting with 0 (leftmost needle is 0). */
  def index: Int
  /** Number as shown on the machine (-100 to 100, 0 does not exist, negative are yellow). */
  def number: Int
}

object Needle {
  def needleCount = 200

  def atIndex(index: Int): Needle = {
    require(index >= 0)
    require(index < needleCount)
    NeedleImpl(index)
  }

  private case class NeedleImpl(index: Int) extends Needle {
    override def number = {
      if (index < 100) -100 + index
      else index - 99
    }
    override def toString = s"Needle($index)"
  }

}