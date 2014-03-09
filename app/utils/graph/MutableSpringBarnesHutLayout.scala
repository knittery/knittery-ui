package utils.graph

import scala.annotation.tailrec
import scalax.collection._
import GraphPredef._
import utils.vector._
import scala.collection.immutable.VectorBuilder

object MutableSpringBarnesHutLayout {
  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], in: Box3, theta: Double): IncrementalLayout[N] =
    apply(graph, _ => Vector3.random(in), theta)

  def apply[N, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], positions: Layout[N], theta: Double): IncrementalLayout[N] = {
    val in = Box3.containing(graph.nodes.map(_.value).map(positions))
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
    val nodePos = graph.nodes.map(n => positions(n.value))

    new MutableSpringBarnesHutLayout(nodeMap, springs.toVector, nodePos.toVector)
  }

  private class MutableSpringBarnesHutLayout[N](
    lookupMap: Map[N, Int],
    springs: Vector[Spring],
    positions: Vector[Vector3])(
      implicit repulsionConstant: RepulsionConstant,
      epsilon: Epsilon,
      mac: MultipoleAcceptanceCriterion) extends IncrementalLayout[N] {

    def apply(n: N) = positions(lookupMap(n))

    def improve = {
      val forces = positions.map(_.toMutable)
      attract(forces)
      repulse(forces)
      new MutableSpringBarnesHutLayout(lookupMap, springs, forces.map(_.toVector3).toVector)
    }

    private def attract(forces: IndexedSeq[MutableVector3]) = {
      springs.foreach { spring =>
        val force = spring.force(positions(spring.node1), positions(spring.node2))
        forces(spring.node1) -= force
        forces(spring.node2) += force
      }
    }

    private def repulse(forces: IndexedSeq[MutableVector3]) = {
      val bodies = positions.map(Body)
      val oct = Oct(Box3.containing(positions))
      bodies.foreach(oct += _)

      (0 until forces.size).par.foreach { i =>
        forces(i) += oct.force(bodies(i))
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
    def centerOfMass: Vector3
    def distance(to: Node) = (centerOfMass - to.centerOfMass).length
    def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon, mac: MultipoleAcceptanceCriterion): Vector3
  }
  private case class Body(centerOfMass: Vector3) extends Node {
    override def mass = 1
    def applyForce(f: Vector3) = copy(centerOfMass = centerOfMass + f)
    override def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon, mac: MultipoleAcceptanceCriterion) = {
      val vec = (against.centerOfMass - centerOfMass)
      val distance = vec.length
      vec * (repulsionConstant.value / (distance * distance * distance + epsilon.value))
    }
  }
  private case object Empty extends Node {
    override def mass = 0d
    override def centerOfMass = Vector3.zero
    override def force(against: Body)(implicit repulsionConstant: RepulsionConstant, epsilon: Epsilon, mac: MultipoleAcceptanceCriterion) =
      Vector3.zero
  }

  private class Oct(bounds: Box3) extends Node {
    val children = Array[Node](Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty)

    lazy val center = bounds.center
    override lazy val mass = children.view.map(_.mass).sum
    override lazy val centerOfMass =
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
        val v = children(0).force(body).toMutable
        v += children(1).force(body)
        v += children(2).force(body)
        v += children(3).force(body)
        v += children(4).force(body)
        v += children(5).force(body)
        v += children(6).force(body)
        v.toVector3
      }
    }

    def +=(body: Body): Unit = {
      val p = body.centerOfMass
      val index = (if (p.x < center.x) 0 else 1) +
        (if (p.y < center.y) 0 else 2) +
        (if (p.z < center.z) 0 else 4)
      children(index) match {
        case Empty => children(index) = body
        case oct: Oct => oct += body
        case other: Body =>
          val origin = Vector3(
            if (p.x < center.x) bounds.origin.x else center.x,
            if (p.y < center.y) bounds.origin.y else center.y,
            if (p.z < center.z) bounds.origin.z else center.z)
          val noct = new Oct(Box3(origin, bounds.size / 2))
          noct += body
          noct += other
          children(index) = noct
      }
    }
  }
  private object Oct {
    def apply(bounds: Box3) = {
      //make bounds the same size in each dimension
      val size = bounds.size.x max bounds.size.y max bounds.size.z
      new Oct(bounds.copy(size = Vector3(size, size, size)))
    }
  }

  private case class Spring(node1: Int, node2: Int, strength: Double, springConstant: Double) {
    private val factor = springConstant * strength
    def force(nodeA: Vector3, nodeB: Vector3) = (nodeA - nodeB) * factor
  }
}