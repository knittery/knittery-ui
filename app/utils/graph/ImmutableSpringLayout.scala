package utils.graph

import scala.annotation.tailrec
import scalax.collection._
import GraphPredef._

object ImmutableSpringLayout {

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

    new ImmutableSpringLayout(nodeMap, springs.toVector, nodePos.toVector)(repulsionConstant, epsilon)
  }

  private class ImmutableSpringLayout[N](
    lookupMap: Map[N, Int],
    springs: Vector[Spring],
    positions: Vector[Vec3])(
      implicit repulsionConstant: RepulsionConstant,
      epsilon: Epsilon) extends IncrementalLayout[N] {

    def apply(n: N) = positions(lookupMap(n)).toVector3

    def improve = {
      val f = (repulse _).andThen(attract)
      new ImmutableSpringLayout(lookupMap, springs, f(positions))
    }

    def attract(forces: Seq[Vec3]) = springs.foldLeft(positions) {
      case (pos2, spring) =>
        val force = spring.force(positions(spring.node1), positions(spring.node2))
        pos2
          .updated(spring.node1, pos2(spring.node1) - force)
          .updated(spring.node2, pos2(spring.node2) + force)
    }

    def repulse(forces: Seq[Vec3]) = {
      val bodies = positions.map(pos => Body(pos))
      bodies.zip(forces) map {
        case (body, force) =>
          bodies.foldLeft(force) { (force, other) =>
            force + body.force(other)
          }
      }
    }
  }

  private case class RepulsionConstant(value: Double) extends AnyVal
  private case class Epsilon(value: Double) extends AnyVal
  private case class Body(centerOfMass: Vec3) extends AnyVal {
    def mass = 1
    def distance(to: Body) = (centerOfMass - to.centerOfMass).length
    def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon) = {
      val vec = (centerOfMass - against.centerOfMass)
      val distance = vec.length
      vec * (repulsionConstant.value * mass * against.mass / (distance * distance * distance + epsilon.value))
    }
    def applyForce(f: Vec3) = copy(centerOfMass = centerOfMass + f)
  }
  private case class Spring(node1: Int, node2: Int, strength: Double, springConstant: Double) {
    private val factor = springConstant * strength
    def force(nodeA: Vec3, nodeB: Vec3) = (nodeA - nodeB) * factor
  }
}