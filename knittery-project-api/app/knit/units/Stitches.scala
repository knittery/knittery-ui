package knit.units

import squants.space.Length
import Stitches.NumericStitches

/** Unit for stitches. */
case class Stitches(amount: Double) {
  def approx = amount.round.toInt
  def discardPartials = Stitches(approx)

  def +(o: Stitches) = Stitches(amount + o.amount)
  def -(o: Stitches) = Stitches(amount - o.amount)
  def *(factor: Double) = Stitches(amount * factor)
  def /(d: Double) = this * (1d / d)
  def /(o: Stitches) = amount / o.amount
  def %(o: Stitches) = Stitches(amount % o.amount)
  def %(d: Double) = Stitches(amount % d)

  def max(o: Stitches) = NumericStitches.max(this, o)
  def min(o: Stitches) = NumericStitches.min(this, o)

  def toLength(implicit gauge: Gauge): Length = gauge.lengthOf(this)

  override def toString = approx.toString
}
object Stitches {
  val zero = NumericStitches.zero

  object NumericStitches extends Numeric[Stitches] {
    def plus(x: Stitches, y: Stitches) = x + y
    def toDouble(x: Stitches) = x.amount
    def toFloat(x: Stitches) = x.amount.toFloat
    def toInt(x: Stitches) = x.approx
    def toLong(x: Stitches) = x.approx
    def fromInt(x: Int) = Stitches(x)
    def negate(x: Stitches) = Stitches(0) - x
    def minus(x: Stitches, y: Stitches) = x - y
    def times(x: Stitches, y: Stitches) = throw new UnsupportedOperationException("Numeric.times not supported for stitches")
    def compare(x: Stitches, y: Stitches) = x.amount.compare(y.amount)
  }
}
