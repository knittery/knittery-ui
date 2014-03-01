package utils.graph

import scala.annotation.tailrec
import scalax.collection._
import GraphPredef._

object SpringBarnesHutLayout {
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

    new SpringBarnesHutLayout(nodeMap, springs.toVector, nodePos.toVector)(repulsionConstant, epsilon)
  }

  private class SpringBarnesHutLayout[N](
    lookupMap: Map[N, Int],
    springs: Vector[Spring],
    positions: Vector[Vec3])(
      implicit repulsionConstant: RepulsionConstant,
      epsilon: Epsilon) extends IncrementalLayout[N] {

    def apply(n: N) = positions(lookupMap(n)).toVector3

    def improve = {
      val f = (attract _).andThen(repulse)
      new SpringBarnesHutLayout(lookupMap, springs, f(positions))
    }

    private def attract(forces: Vector[Vec3]) = springs.foldLeft(forces) {
      case (forces, spring) =>
        val force = spring.force(positions(spring.node1), positions(spring.node2))
        forces
          .updated(spring.node1, forces(spring.node1) - force)
          .updated(spring.node2, forces(spring.node2) + force)
    }

    private def repulse(forces: Vector[Vec3]) = {
      val bodies = positions.map(Body)
      val oct = bodies.foldLeft(Oct(Box3.containing(positions)))(_ + _)

      forces
    }
  }

  private case class RepulsionConstant(value: Double) extends AnyVal
  private case class Epsilon(value: Double) extends AnyVal

  private sealed trait Node {
    def mass: Double
    def centerOfMass: Vec3
    def distance(to: Node) = (centerOfMass - to.centerOfMass).length
    def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon) = {
      val vec = (centerOfMass - against.centerOfMass)
      val distance = vec.length
      vec * (repulsionConstant.value * mass * against.mass / (distance * distance * distance + epsilon.value))
    }
  }
  private case class Body(centerOfMass: Vec3) extends Node {
    override def mass = 1
    def applyForce(f: Vec3) = copy(centerOfMass = centerOfMass + f)
  }
  private case object Empty extends Node {
    override val mass = 0d
    override val centerOfMass = Vec3.zero
  }
  private case class Oct private (
    bounds: Box3,
    children: Vector[Node]) extends Node {
    def center = bounds.center
    override def mass = children.view.map(_.mass).sum
    override def centerOfMass =
      children.view.map(n => n.centerOfMass * n.mass).reduce(_ + _) / mass
    def +(body: Body): Oct = {
      val p = body.centerOfMass
      val index = (if (p.x < center.x) 0 else 1) +
        (if (p.y < center.y) 0 else 2) +
        (if (p.z < center.z) 0 else 4)
      val newC = children(index) match {
        case Empty => body
        case oct: Oct => oct + body
        case other: Body => Oct(bounds) + body + other
      }
      copy(children = children.updated(index, newC))
    }
  }
  private object Oct {
    def apply(bounds: Box3): Oct = Oct(bounds, emptyVector)
    private val emptyVector = (0 until 8).map(_ => Empty: Node).toVector
  }

  private case class Spring(node1: Int, node2: Int, strength: Double, springConstant: Double) {
    private val factor = springConstant * strength
    def force(nodeA: Vec3, nodeB: Vec3) = (nodeA - nodeB) * factor
  }
}