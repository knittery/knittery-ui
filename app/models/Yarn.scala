package models

import java.util.concurrent.atomic.AtomicLong
import java.awt.Color
import utils.ConsoleColors
import scala.annotation.tailrec

case class Yarn(name: String, color: Color) {
  def consoleColor = ConsoleColors.closest(color)
  override def toString = name
}
object Yarn {
  val contrastYarn = Yarn("Contrast", Color.cyan)
}

sealed trait YarnFlow {
  def yarn: Yarn
  def next(distance: Int) = new YarnPoint(this, distance) {}
  def nexts(distance: Int): Stream[YarnPoint] = {
    val n = next(distance)
    n #:: n.nexts(distance)
  }
  def previous: Stream[YarnFlow]
  /** This element and all the previous elements. */
  def stream = this #:: previous
  def start: YarnPiece
  /** length of the yarn (sum of all distances). */
  def length: Int
  def contains(other: YarnFlow): Boolean
}
sealed abstract class YarnPoint(val prev: YarnFlow, val distance: Int) extends YarnFlow {
  require(distance >= 0)
  override def yarn = start.yarn
  override def previous = prev.stream
  override val start = prev.start
  override val length = prev.length + distance
  override def contains(other: YarnFlow) =
    start == other.start && length >= other.length && inStream(other)
  @tailrec private def inStream(other: YarnFlow): Boolean = {
    if (this == other) true
    else if (length < other.length) false
    else prev match {
      case p: YarnPoint => p.inStream(other)
      case p => p.contains(other)
    }
  }
  override def hashCode = start.hashCode ^ length
  override def equals(o: Any) = o match {
    case other: YarnPoint => start == other.start && length == other.length
    case _ => false
  }
  override def toString = s"$start+$length"
}
class YarnPiece(val yarn: Yarn, id: Long) extends YarnFlow {
  override def previous = Stream.empty
  override def start = YarnPiece.this
  override def length = 0
  override def contains(other: YarnFlow) = YarnPiece.this == other
  //equals must be for object identity
  override def toString = s"$id#$yarn"
}
object YarnPiece {
  def apply(yarn: Yarn) = new YarnPiece(yarn, ids.incrementAndGet)
  private val ids = new AtomicLong
}