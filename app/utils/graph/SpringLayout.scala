package utils.graph

import scalax.collection._
import GraphPredef._

object SpringLayout {
  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], in: Box): IncrementalLayout[N] = {
    new SpringLayout[N] {
      val nodes = graph.nodes.map(_.value).zipWithIndex.toMap
      val positions = graph.nodes.map(_ => Vector3.random(in)).toArray
      val forces = graph.nodes.map(_ => Vector3.zero).toArray
      val connections = graph.edges.map { e =>
        Connection(nodes(e._1.value), nodes(e._2.value), e.weight)
      }.toVector
      val size = in.size
    }
  }

  private case class Connection(index1: Int, index2: Int, weight: Double)

  private trait SpringLayout[N] extends IncrementalLayout[N] {
    protected val nodes: Map[N, Int]
    protected val positions: Array[Vector3]
    protected val forces: Array[Vector3]
    protected val connections: Traversable[Connection]
    def count = forces.length

    def size: Vector3
    def spring = 1d / 5
    def repulsion = 1d

    lazy val repulsionConstant = {
      val density = Math.pow(size.volume / count, 1d / 3)
      Math.pow(repulsion * density, 2)
    }
    lazy val springConstant = {
      val maxWeight = connections.map(_.weight).max
      spring / maxWeight
    }

    def apply(node: N) = {
      nodes.get(node).map(positions).
        getOrElse(throw new IllegalArgumentException(s"$node is not part of the layout"))
    }

    def improve() = {
      repulse()
      attract()
      move()
    }

    protected def repulse() = {
      val const = repulsionConstant
      val epsilon = size.length / 10000000
      for (i <- 0 until count; j <- i + 1 until count) {
        val v = positions(i) - positions(j)
        val distance = v.length + epsilon
        v *= const / (distance * distance * distance)
        forces(i) += v
        forces(j) -= v
      }
    }
    protected def attract() = {
      val const = springConstant
      for (conn <- connections) {
        val v = positions(conn.index1) - positions(conn.index2)
        v *= const
        forces(conn.index1) -= v
        forces(conn.index2) += v
      }
    }
    protected def move() = {
      var total = 0d
      for (i <- 0 until count) {
        val v = forces(i)
        positions(i) += v
        total += v.length
        v.zero()
      }
      total
    }
  }
}