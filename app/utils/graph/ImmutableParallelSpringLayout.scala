package utils.graph

import scala.annotation.tailrec
import scalax.collection._
import GraphPredef._
import utils.vector._

object ImmutableParallelSpringLayout {
  def apply[N, E[X] <: EdgeLikeIn[X]](graph: Graph[N, E], in: Box3): IncrementalLayout[N] =
    apply(graph, _ => Vector3.random(in))

  def apply[N, E[X] <: EdgeLikeIn[X]](graph: Graph[N, E], positions: Layout[N]): IncrementalLayout[N] = {
    val in = Box3.containing(graph.nodes.map(_.value).map(positions))
    val springConstant = 1d / (graph.edges.map(_.weight).max * 5)
    val repulsionConstant = RepulsionConstant {
      val density = Math.pow(in.size.volume / graph.size, 1d / 3)
      density * density
    }
    val epsilon = Epsilon(in.size.length / 10000000)

    val nodes = graph.nodes.toVector
    val nodeMap = nodes.map(_.value).zipWithIndex.toMap
    val springs = graph.edges.map { e =>
      Spring(nodeMap(e._1.value), nodeMap(e._2.value), e.weight, springConstant)
    }
    val nodePos = nodes.map(n => positions(n))

    new ImmutableParallelSpringLayout(nodeMap, springs.toVector, nodePos.toVector)(repulsionConstant, epsilon)
  }

  private class ImmutableParallelSpringLayout[N](
    lookupMap: Map[N, Int],
    springs: Vector[Spring],
    positions: Vector[Vector3])(
      implicit repulsionConstant: RepulsionConstant,
      epsilon: Epsilon) extends IncrementalLayout[N] {

    def apply(n: N) = positions(lookupMap(n))

    def improve = {
      val f = (attract _).andThen(repulse)
      new ImmutableParallelSpringLayout(lookupMap, springs, f(positions))
    }

    def attract(forces: Vector[Vector3]) = springs.foldLeft(forces) {
      case (result, spring) =>
        val force = spring.force(positions(spring.node1), positions(spring.node2))
        result
          .updated(spring.node1, result(spring.node1) - force)
          .updated(spring.node2, result(spring.node2) + force)
    }

    def repulse(forces: Vector[Vector3]) = {
      val bodies = genericArrayOps(positions.map(pos => Body(pos)).toArray).toSeq

      // Parallel implementation
      bodies.zip(forces).par.map {
        case (body, force) =>
          //The immutable variant would be:
          //   bodies.foldLeft(force) { (force, other) =>
          //     force + body.force(other)
          //   }
          //However this one is faster by almost a factor of two
          var forceV = force.toMutable
          bodies.foreach { forceV += body.force(_) }
          forceV.toVector3
      }.toVector
    }
  }

  private case class RepulsionConstant(value: Double) extends AnyVal
  private case class Epsilon(value: Double) extends AnyVal
  private case class Body(centerOfMass: Vector3) extends AnyVal {
    def distance(to: Body) = (centerOfMass - to.centerOfMass).length
    def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon) = {
      val vec = centerOfMass - against.centerOfMass
      val distance = vec.length
      vec * (repulsionConstant.value / (distance * distance * distance + epsilon.value))
    }
  }
  private case class Spring(node1: Int, node2: Int, strength: Double, springConstant: Double) {
    private val factor = springConstant * strength
    def force(nodeA: Vector3, nodeB: Vector3) = (nodeA - nodeB) * factor
  }
}