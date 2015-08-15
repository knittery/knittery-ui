package knit.gauge

import knit.Tension
import knit.units.{Stitches, Rows, Gauge}
import squants.space.Length
import squants.space.LengthConversions._

/** Standardized simple 10cmx10cm gauge by counting the stitches and rows. */
case class StandardGauge(stitchesFor10Cm: Int, rowsFor10cm: Int, tension: Tension) extends Gauge {
  def rowsFor(l: Length) = {
    val count = (l / 10.cm) * rowsFor10cm
    Rows(count)
  }

  def stitchesFor(l: Length) = {
    val count = (l / 10.cm) * stitchesFor10Cm
    Stitches(count)
  }
}

/** Gauge where some distance is measured and the stitches/rows in it are counted. */
case class MeasuredGauge(stitches: Stitches, width: Length, rows: Rows, length: Length, tension: Tension) extends Gauge {
  def rowsFor(l: Length) = rows * (l / length)
  def stitchesFor(l: Length) = stitches * (l / width)
}
