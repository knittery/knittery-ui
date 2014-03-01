package utils.graph

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scalax.collection._
import GraphPredef._
import utils.vector._

object ImmutableSpringLayout {
  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], in: Box): IncrementalLayout[N] =
    apply(graph, _ => Vector3.random(in).toMutable)

  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], positions: Layout[N]): IncrementalLayout[N] = {
    val in = Box3.containing(graph.nodes.map(_.value).map(positions).map(_.toVector3))
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
    val nodePos = graph.nodes.map(n => positions(n).toVector3)
    new ImmutableSpringLayout(nodeMap, springs.toVector, nodePos.toVector)(repulsionConstant, epsilon)
  }

  private class ImmutableSpringLayout[N](
    lookupMap: Map[N, Int],
    springs: Vector[Spring],
    positions: Vector[Vector3])(
      implicit repulsionConstant: RepulsionConstant,
      epsilon: Epsilon) extends IncrementalLayout[N] {

    def apply(n: N) = positions(lookupMap(n)).toMutable

    def improve = {
      val f = (attract _).andThen(repulse)
      new ImmutableSpringLayout(lookupMap, springs, f(positions))
    }

    def attract(forces: Vector[Vector3]) = springs.foldLeft(forces) {
      case (forces, spring) =>
        val force = spring.force(positions(spring.node1), positions(spring.node2))
        forces
          .updated(spring.node1, forces(spring.node1) - force)
          .updated(spring.node2, forces(spring.node2) + force)
    }

    def repulse(forces: Vector[Vector3]) = {
      val bodies = genericArrayOps(positions.map(pos => Body(pos)).toArray).toSeq

      //The fastest single threaded implementation
      @tailrec
      def repulseBodies(remainingBodies: List[Body], forces: List[Vector3], out: VectorBuilder[Vector3]): Vector[Vector3] = {
        if (remainingBodies.isEmpty) out.result
        else {
          val body = remainingBodies.head
          var force = forces.head.toMutable
          bodies.foreach { other =>
            force += body.force(other)
          }
          repulseBodies(remainingBodies.tail, forces.tail, out += force.toVector3)
        }
      }
      repulseBodies(bodies.toList, forces.toList, new VectorBuilder).toVector
    }
  }

  private case class RepulsionConstant(value: Double) extends AnyVal
  private case class Epsilon(value: Double) extends AnyVal
  private case class Body(centerOfMass: Vector3) extends AnyVal {
    def distance(to: Body) = (centerOfMass - to.centerOfMass).length
    def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon) = {
      val vec = (centerOfMass - against.centerOfMass)
      val distance = vec.length
      vec * (repulsionConstant.value / (distance * distance * distance + epsilon.value))
    }
  }
  private case class Spring(node1: Int, node2: Int, strength: Double, springConstant: Double) {
    private val factor = springConstant * strength
    def force(nodeA: Vector3, nodeB: Vector3) = (nodeA - nodeB) * factor
  }
}