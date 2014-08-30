package models.gauge

import squants.space.Length
import squants.space.LengthConversions._

/** Standardized simple 10cmx10cm gauge by counting the stitches and rows. */
case class StandardGauge(stitchesFor10Cm: Int, rowsFor10cm: Int) extends Gauge {
  def rowsFor(d: Length) = {
    val count = (d / 10.cm) * rowsFor10cm
    count.round.toInt
  }

  def stitchesFor(d: Length) = {
    val count = (d / 10.cm) * stitchesFor10Cm
    count.round.toInt
  }
}
