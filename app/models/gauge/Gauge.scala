package models.gauge

import squants.space.Length

/** Maps stitch and row count to metric length. */
trait Gauge {
  def rowsFor(d: Length): Int
  def stitchesFor(d: Length): Int
}