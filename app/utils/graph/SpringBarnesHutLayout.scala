package utils.graph

import scala.annotation.tailrec
import scalax.collection._
import GraphPredef._

object SpringBarnesHutLayout {

  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], in: Box): IncrementalLayout[N] = {
    val nodes = graph.nodes.map(n => (n.value, Vector3.random(in))).toMap
    val edges = graph.edges.map(e => (e._1.value, e._2.value, e.weight.toDouble))
    new SBHLayout(in.size)(nodes, edges)
  }

  private class SBHLayout[N](size: Vector3)(nodes: Map[N, Position], edges: Traversable[(N, N, Double)]) extends IncrementalLayout[N] {
    def apply(n: N) = nodes(n)

    def improve = {
      val attracted = edges.foldLeft(nodes.mapValues(Vec.apply)) {
        case (nodeMap, (a, b, weight)) =>
          val nodeA = nodeMap(a)
          val nodeB = nodeMap(b)
          val force = (nodeA - nodeB) * (springConstant * weight)
          nodeMap + ((a, nodeA - force)) + ((b, nodeB + force))
      }

      val bodies = attracted.map(x => (Body(x._2, x._1))).toVector
      val repulsedAndAttracted = bodies.map { node =>
        val newPosition = bodies.foldLeft(node.centerOfMass) {
          case (pos, o) => pos + node.force(o)
        }
        node.copy(centerOfMass = newPosition)
      }.seq

      new SBHLayout(size)(
        repulsedAndAttracted.map(x => x.value -> x.centerOfMass.toVector3).toMap,
        edges)
    }

    val repulsionConstant = {
      val density = Math.pow(size.volume / nodes.size, 1d / 3)
      Math.pow(density, 2)
    }
    val springConstant = {
      val maxWeight = edges.map(_._3).max
      1 / (maxWeight * 5)
    }

    type F = Double
    case class Vec(x: F, y: F, z: F) {
      def +(o: Vec) = Vec(x + o.x, y + o.y, z + o.z)
      def -(o: Vec) = Vec(x - o.x, y - o.y, z - o.z)
      def *(scalar: F) = Vec(x * scalar, y * scalar, z * scalar)
      def /(scalar: F) = Vec(x / scalar, y / scalar, z / scalar)
      def unary_- = Vec(-x, -y, -z)
      def length = Math.sqrt(x * x + y * y + z * z)
      def toVector3 = Vector3(x, y, z)
      override def toString = s"($x,$y,$z)"
    }
    object Vec {
      val zero = Vec(0, 0, 0)
      def apply(v: Vector3): Vec = Vec(v.x, v.y, v.z)
    }

    val epsilon = size.length / 10000000
    sealed trait Node {
      def centerOfMass: Vec
      def mass: F
      def distance(to: Node) = (centerOfMass - to.centerOfMass).length
      def force(against: Node) = {
        val vec = (centerOfMass - against.centerOfMass)
        val distance = vec.length
        vec * (repulsionConstant * mass * against.mass / (distance * distance * distance + epsilon))
      }
    }
    case class Body(centerOfMass: Vec, value: N) extends Node {
      def mass = 1
      def applyForce(f: Vec) = copy(centerOfMass = centerOfMass + f)
    }

    case class Spring(a: Node, b: Node, strength: Double) {
      def force = {
        val vec = a.centerOfMass - b.centerOfMass
        vec * (springConstant * strength)
      }
    }
  }
}