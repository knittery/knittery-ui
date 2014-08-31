package models.gauge

import models.Tension
import models.units.{Stitches, Rows, Gauge}
import squants.space.Length
import squants.space.LengthConversions._

/** Standardized simple 10cmx10cm gauge by counting the stitches and rows. */
case class StandardGauge(stitchesFor10Cm: Int, rowsFor10cm: Int, tension: Tension) extends Gauge {
  def rowsFor(d: Length) = {
    val count = (d / 10.cm) * rowsFor10cm
    Rows(count)
  }

  def stitchesFor(d: Length) = {
    val count = (d / 10.cm) * stitchesFor10Cm
    Stitches(count)
  }
}
