package utils.graph

import scala.annotation.tailrec
import scalax.collection._
import GraphPredef._

object SpringBarnesHutLayout {
  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], in: Box, theta: Double): IncrementalLayout[N] =
    apply(graph, _ => Vec3.random(in).toVector3, theta)

  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], positions: Layout[N], theta: Double): IncrementalLayout[N] = {
    val in = Box3.containing(graph.nodes.map(_.value).map(positions).map(_.toVec3))
    val springConstant = 1d / (graph.edges.map(_.weight).max * 5)
    implicit val repulsionConstant = RepulsionConstant {
      val density = Math.pow(in.size.volume / graph.size, 1d / 3)
      density * density
    }
    implicit val epsilon = Epsilon(in.size.length / 10000000)
    implicit val mac = MultipoleAcceptanceCriterion(theta)

    val nodeMap = graph.nodes.map(_.value).zipWithIndex.toMap
    val springs = graph.edges.map { e =>
      Spring(nodeMap(e._1.value), nodeMap(e._2.value), e.weight, springConstant)
    }
    val nodePos = graph.nodes.map(n => positions(n.value).toVec3)

    new SpringBarnesHutLayout(nodeMap, springs.toVector, nodePos.toVector)
  }

  private class SpringBarnesHutLayout[N](
    lookupMap: Map[N, Int],
    springs: Vector[Spring],
    positions: Vector[Vec3])(
      implicit repulsionConstant: RepulsionConstant,
      epsilon: Epsilon,
      mac: MultipoleAcceptanceCriterion) extends IncrementalLayout[N] {

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

      bodies.zip(forces).map {
        case (body, force) =>
          force + oct.force(body)
      }
    }
  }

  private case class RepulsionConstant(value: Double) extends AnyVal
  private case class Epsilon(value: Double) extends AnyVal
  private case class MultipoleAcceptanceCriterion(value: Double) extends AnyVal {
    def accepts(boxSize: Double, distance: Double) = boxSize / distance < value
  }

  private sealed trait Node {
    def mass: Double
    def centerOfMass: Vec3
    def distance(to: Node) = (centerOfMass - to.centerOfMass).length
    def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon, mac: MultipoleAcceptanceCriterion): Vec3
  }
  private case class Body(centerOfMass: Vec3) extends Node {
    override def mass = 1
    def applyForce(f: Vec3) = copy(centerOfMass = centerOfMass + f)
    override def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon, mac: MultipoleAcceptanceCriterion) = {
      val vec = (against.centerOfMass - centerOfMass)
      val distance = vec.length
      vec * (repulsionConstant.value / (distance * distance * distance + epsilon.value))
    }
  }
  private case object Empty extends Node {
    override val mass = 0d
    override val centerOfMass = Vec3.zero
    override def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon, mac: MultipoleAcceptanceCriterion) =
      Vec3.zero
  }
  private case class Oct private (
    bounds: Box3,
    children: Vector[Node]) extends Node {
    def center = bounds.center
    override val mass = children.view.map(_.mass).sum
    override val centerOfMass =
      children.view.map(n => n.centerOfMass * n.mass).reduce(_ + _) / mass
    def size = bounds.size.x //same size in each direction

    override def force(body: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon, mac: MultipoleAcceptanceCriterion) = {
      val vec = (body.centerOfMass - centerOfMass)
      val distance = vec.length
      if (mac.accepts(size, distance)) {
        // distance is big enough so we can threat ourself as a cluster regarding body
        vec * (repulsionConstant.value * mass / (distance * distance * distance + epsilon.value))
      } else {
        // need to calculate the force for each child
        val v = children(0).force(body).toVector3
        v += children(1).force(body)
        v += children(2).force(body)
        v += children(3).force(body)
        v += children(4).force(body)
        v += children(5).force(body)
        v += children(6).force(body)
        v.toVec3
      }
    }

    def +(body: Body): Oct = {
      val p = body.centerOfMass
      val index = (if (p.x < center.x) 0 else 1) +
        (if (p.y < center.y) 0 else 2) +
        (if (p.z < center.z) 0 else 4)
      val newC = children(index) match {
        case Empty => body
        case oct: Oct => oct + body
        case other: Body =>
          val origin = Vec3(
            if (p.x < center.x) bounds.origin.x else center.x,
            if (p.y < center.y) bounds.origin.y else center.y,
            if (p.z < center.z) bounds.origin.z else center.z)
          Oct(Box3(origin, bounds.size / 2)) + body + other
      }
      copy(children = children.updated(index, newC))
    }
  }
  private object Oct {
    def apply(bounds: Box3): Oct = {
      //make bounds the same size in each dimension
      val size = bounds.size.x max bounds.size.y max bounds.size.z
      Oct(bounds.copy(size = Vec3(size, size, size)), emptyVector)
    }
    private val emptyVector = (0 until 8).map(_ => Empty: Node).toVector
  }

  private case class Spring(node1: Int, node2: Int, strength: Double, springConstant: Double) {
    private val factor = springConstant * strength
    def force(nodeA: Vec3, nodeB: Vec3) = (nodeA - nodeB) * factor
  }
}