package utils.graph

import scalax.collection._
import GraphPredef._
import utils.vector._

object SpringLayout {
  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], in: Box3): IncrementalLayout[N] = {
    val nodes = graph.nodes.toVector
    new SpringLayout[N] {
      val nodeMap = nodes.map(_.value).zipWithIndex.toMap
      val positions = nodes.map(_ => Vector3.random(in).toMutable).toArray
      val forces = nodes.map(_ => MutableVector3.zero).toArray
      val connections = graph.edges.map { e =>
        Connection(nodeMap(e._1.value), nodeMap(e._2.value), e.weight)
      }.toVector
      val size = in.size
    }
  }

  private case class Connection(index1: Int, index2: Int, weight: Double)

  private trait SpringLayout[N] extends IncrementalLayout[N] {
    protected val nodeMap: Map[N, Int]
    protected val positions: Array[MutableVector3]
    protected val forces: Array[MutableVector3]
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
      nodeMap.get(node).map(n => positions(n).toVector3).
        getOrElse(throw new IllegalArgumentException(s"$node is not part of the layout"))
    }

    def improve() = {
      repulse()
      attract()
      move()
      this
    }

    protected def repulse() = {
      val const = repulsionConstant
      val epsilon = size.length / 10000000
      var i = 0
      var j = 0
      while (i < count) {
        j = i + 1
        while (j < count) {
          val v = positions(i) - positions(j)
          val distance = v.length + epsilon
          v *= const / (distance * distance * distance)
          forces(i) += v
          forces(j) -= v
          j = j + 1
        }
        i = i + 1
      }
    }
    protected def attract() = {
      val const = springConstant
      for (conn <- connections) {
        val v = positions(conn.index1) - positions(conn.index2)
        v *= const * conn.weight
        forces(conn.index1) -= v
        forces(conn.index2) += v
      }
    }
    protected def move() = {
      var i = 0
      while (i < count) {
        val v = forces(i)
        positions(i) += v
        v.zero()
        i = i + 1
      }
    }
  }
}