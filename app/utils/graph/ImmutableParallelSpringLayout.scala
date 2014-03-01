package utils.graph

import scala.annotation.tailrec
import scalax.collection._
import GraphPredef._

object ImmutableParallelSpringLayout {

  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], in: Box): IncrementalLayout[N] = {
    val springConstant = 1d / (graph.edges.map(_.weight).max * 5)
    val repulsionConstant = RepulsionConstant {
      val density = Math.pow(in.size.volume / graph.size, 1d / 3)
      density * density
    }
    val epsilon = Epsilon(in.size.length / 10000000)

    val nodeMap = graph.nodes.map(_.value).zipWithIndex.toMap
    val springs = graph.edges.map { e =>
      Spring(nodeMap(e._1.value), nodeMap(e._2.value), e.weight, springConstant)
    }
    val nodePos = graph.nodes.map(_ => Vector3.random(in).toVec3)

    new ImmutableParallelSpringLayout(nodeMap, springs.toVector, nodePos.toVector)(repulsionConstant, epsilon)
  }

  private class ImmutableParallelSpringLayout[N](
    lookupMap: Map[N, Int],
    springs: Vector[Spring],
    positions: Vector[Vec3])(
      implicit repulsionConstant: RepulsionConstant,
      epsilon: Epsilon) extends IncrementalLayout[N] {

    def apply(n: N) = positions(lookupMap(n)).toVector3

    def improve = {
      val f = (attract _).andThen(repulse)
      new ImmutableParallelSpringLayout(lookupMap, springs, f(positions))
    }

    def attract(forces: Vector[Vec3]) = springs.foldLeft(forces) {
      case (forces, spring) =>
        val force = spring.force(positions(spring.node1), positions(spring.node2))
        forces
          .updated(spring.node1, forces(spring.node1) - force)
          .updated(spring.node2, forces(spring.node2) + force)
    }

    def repulse(forces: Vector[Vec3]) = {
      val bodies = genericArrayOps(positions.map(pos => Body(pos)).toArray).toSeq

      // Parallel implementation
      bodies.zip(forces).par.map {
        case (body, force) =>
          //The immutable variant would be:
          //   bodies.foldLeft(force) { (force, other) =>
          //     force + body.force(other)
          //   }
          //How ever this one is faster by almost a factor of two
          var forceV = force.toVector3
          bodies.foreach { forceV += body.force(_) }
          forceV.toVec3
      }.toVector
    }
  }

  private case class RepulsionConstant(value: Double) extends AnyVal
  private case class Epsilon(value: Double) extends AnyVal
  private case class Body(centerOfMass: Vec3) extends AnyVal {
    def distance(to: Body) = (centerOfMass - to.centerOfMass).length
    def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon) = {
      val vec = (centerOfMass - against.centerOfMass)
      val distance = vec.length
      vec * (repulsionConstant.value / (distance * distance * distance + epsilon.value))
    }
    def applyForce(f: Vec3) = copy(centerOfMass = centerOfMass + f)
  }
  private case class Spring(node1: Int, node2: Int, strength: Double, springConstant: Double) {
    private val factor = springConstant * strength
    def force(nodeA: Vec3, nodeB: Vec3) = (nodeA - nodeB) * factor
  }
}