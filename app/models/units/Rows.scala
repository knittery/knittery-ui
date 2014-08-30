package models.units

import models.units.Rows.NumericRows
import squants.space.Length

/** Unit for stitched rows. */
case class Rows(amount: Double) {
  def approx = amount.round.toInt
  def discardPartials = Rows(approx)

  def +(o: Rows) = Rows(amount + o.amount)
  def -(o: Rows) = Rows(amount - o.amount)
  def *(factor: Double) = Rows(amount * factor)
  def /(d: Double) = this * (1d / d)
  def /(o: Rows) = amount / o.amount
  def %(o: Rows) = Rows(amount % o.amount)

  def max(o: Rows) = NumericRows.max(this, o)
  def min(o: Rows) = NumericRows.min(this, o)

  def toLength(implicit gauge: Gauge): Length = gauge.lengthOf(this)

  override def toString = approx.toString
}
object Rows {
  val zero = NumericRows.zero

  object NumericRows extends Numeric[Rows] {
    def plus(x: Rows, y: Rows) = x + y
    def toDouble(x: Rows) = x.amount
    def toFloat(x: Rows) = x.amount.toFloat
    def toInt(x: Rows) = x.approx
    def toLong(x: Rows) = x.approx
    def fromInt(x: Int) = Rows(x)
    def negate(x: Rows) = Rows(0) - x
    def minus(x: Rows, y: Rows) = x - y
    def times(x: Rows, y: Rows) = throw new UnsupportedOperationException("Numeric.times not supported for rows")
    def compare(x: Rows, y: Rows) = x.amount.compare(y.amount)
  }
}
