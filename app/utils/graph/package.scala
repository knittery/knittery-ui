package utils

import utils.vector._

package object graph {
  type Position = Vector3

  type Layout[N] = N => Position
  trait IncrementalLayout[N] extends Layout[N] {
    /** Improves the layout. */
    def improve: IncrementalLayout[N]
  }

  case class LayoutOps[N](layout: Layout[N], nodes: Traversable[N]) {
    def positions = nodes.map(layout)
    lazy val bounds = Box3.containing(positions)
    def inside(box: Box3) = {
      val scale = (box.size / bounds.size).minimal
      val scaled = layout.andThen(p => (p - bounds.origin) * scale + box.origin)
      LayoutOps(scaled, nodes).centerOver(box.center)
    }
    def centerOver(v: Vector3) = offset(v - bounds.size/2 - bounds.origin)
    def scale(by: Vector3) = layout.andThen(_ * by)
    def offset(by: Vector3) = layout.andThen(_ + by)
  }

  implicit class RichIncrementalLayout[N](layout: IncrementalLayout[N]) {
    def improves(steps: Int) = {
      require(steps > 0)
      (0 until steps).foldLeft(layout)((l, _) => l.improve)
    }
  }
}