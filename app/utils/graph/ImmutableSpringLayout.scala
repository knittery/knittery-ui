package utils.graph

import scala.annotation.tailrec
import scalax.collection._
import GraphPredef._

object ImmutableSpringLayout {

  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], in: Box): IncrementalLayout[N] = {
    val nodes = graph.nodes.map(n => (n.value, Vector3.random(in))).toMap
    val edges = graph.edges.map(e => (e._1.value, e._2.value, e.weight.toDouble))
    new ImmutableSpringLayout(in.size)(nodes, edges)
  }

  private class ImmutableSpringLayout[N](size: Vector3)(nodes: Map[N, Position], edges: Traversable[(N, N, Double)]) extends IncrementalLayout[N] {
    def apply(n: N) = nodes(n)

    def improve = {
      val attracted = edges.foldLeft(nodes.mapValues(Vec3.apply)) {
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
        (node.value, newPosition.toVector3)
      }.toMap

      new ImmutableSpringLayout(size)(repulsedAndAttracted, edges)
    }

    val repulsionConstant = {
      val density = Math.pow(size.volume / nodes.size, 1d / 3)
      Math.pow(density, 2)
    }
    val springConstant = {
      val maxWeight = edges.map(_._3).max
      1 / (maxWeight * 5)
    }

    val epsilon = size.length / 10000000
    sealed trait Node {
      def centerOfMass: Vec3
      def mass: Double
      def distance(to: Node) = (centerOfMass - to.centerOfMass).length
      def force(against: Node) = {
        val vec = (centerOfMass - against.centerOfMass)
        val distance = vec.length
        vec * (repulsionConstant * mass * against.mass / (distance * distance * distance + epsilon))
      }
    }
    case class Body(centerOfMass: Vec3, value: N) extends Node {
      def mass = 1
      def applyForce(f: Vec3) = copy(centerOfMass = centerOfMass + f)
    }

    case class Spring(a: Node, b: Node, strength: Double) {
      def force = {
        val vec = a.centerOfMass - b.centerOfMass
        vec * (springConstant * strength)
      }
    }
  }
}