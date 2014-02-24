package utils.graph

import scalax.collection.Graph

/** Distributes the nodes at random within a box. */
class RandomLayout {
  def apply[N](nodes: Traversable[N], in: Box): Layout[N] = {
    nodes.map { n => (n, Vector3.random(in)) }.toMap
  }
}