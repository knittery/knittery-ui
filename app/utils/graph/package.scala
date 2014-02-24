package utils

import javax.vecmath.Vector3d
import javax.vecmath.Point3d

package object graph {
  type Position = Vector3

  type Layout[N] = N => Position
  trait IncrementalLayout[N] extends Layout[N] {
    /** Improves the layout. Returns a quality measurement [0,1]. */
    def improve(): Double
  }
}