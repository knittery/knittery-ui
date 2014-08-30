package models.units

import squants.space.Length
import squants.space.LengthConversions._

/** Maps stitch and row count to metric length. */
trait Gauge {
  def rowsFor(d: Length): Rows
  def stitchesFor(d: Length): Stitches

  private def baseLength = 1.meters
  def lengthOf(stitches: Stitches) = baseLength * (stitches / stitchesFor(baseLength))
  def lengthOf(rows: Rows) = baseLength * (rows / rowsFor(baseLength))
}