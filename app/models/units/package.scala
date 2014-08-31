package models

import squants.space.Length

package object units {
  implicit def stitchesNumeric = Stitches.NumericStitches
  implicit class NumericStitchesConverter[A](n: A)(implicit num: scala.Numeric[A]) {
    def stitches = Stitches(num.toDouble(n))
  }
  implicit def rowsNumeric = Stitches.NumericStitches
  implicit class NumericRowsConverter[A](n: A)(implicit num: scala.Numeric[A]) {
    def rows = Rows(num.toDouble(n))
  }


  implicit class LengthConverter(len: Length)(implicit gauge: Gauge) {
    def toStitches = gauge.stitchesFor(len)
    def toRows = gauge.rowsFor(len)
  }
}
