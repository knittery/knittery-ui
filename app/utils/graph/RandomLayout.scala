package utils.graph

import scalax.collection.Graph
import utils.vector._

/** Distributes the nodes at random within a box. */
class RandomLayout {
  def apply[N](nodes: Traversable[N], in: Box): Layout[N] = {
    nodes.map { n => (n, Vec3.random(in).toVector3) }.toMap
  }
}