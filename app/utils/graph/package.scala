package utils

package object graph {
  type Position = Vector3

  type Layout[N] = N => Position
  trait IncrementalLayout[N] extends Layout[N] {
    /** Improves the layout. */
    def improve(): IncrementalLayout[N]
  }
}