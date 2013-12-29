package models

sealed trait Needle {
  /** Numbered from left to right starting with 0 (leftmost needle is 0). */
  def index: Int
  /** Number as shown on the machine (-100 to 100, 0 does not exist, negative are yellow). */
  def number: Int
}

object Needle {
  val count = 200
  val all = (0 until count).map(atIndex)
  val middle = atIndex(count / 2)

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
    override def toString = s"Needle($index)"
  }

}