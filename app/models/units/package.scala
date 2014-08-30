package models

package object units {
  implicit def stitchesNumeric = Stitches.NumericStitches
  implicit class NumericStitchesConverter[A](n: A)(implicit num: scala.Numeric[A]) {
    def stitches = Stitches(num.toDouble(n))
  }
  implicit def rowsNumeric = Stitches.NumericStitches
  implicit class NumericRowsConverter[A](n: A)(implicit num: scala.Numeric[A]) {
    def rows = Rows(num.toDouble(n))
  }


  val a = 10.stitches
  val b = 3.stitches

  val d = 4.rows

  val c = a + b
}
