package utils

import utils.vector._

package object graph {
  type Position = Vector3

  type Layout[N] = N => Position
  trait IncrementalLayout[N] extends Layout[N] {
    /** Improves the layout. */
    def improve(): IncrementalLayout[N]
  }

  implicit class RichIncrementalLayout[N](layout: IncrementalLayout[N]) {
    def improve(steps: Int) = {
      require(steps > 0)
      (0 until steps).foldLeft(layout)((l, _) => l.improve)
    }
  }
}