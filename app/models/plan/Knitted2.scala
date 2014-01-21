package models.plan

import models._

sealed trait Knitted2 {
  val flows: Map[YarnStart, YarnFlow]
  val connections: Set[YarnConnection]

  def +(flow: YarnFlow) = {
    val start = flow.start
    copy(flows = flows.get(start) match {
      case None =>
        flows + (start -> flow)
      case Some(old) if flow.stream.contains(old) =>
        throw new IllegalArgumentException(s"Yarn cannot fork, cannot add $flow, we already contain a different end for $start")
      case Some(_) =>
        flows + (start -> flow)
    })
  }

  def connect(flow1: YarnFlow, flow2: YarnFlow) = {
    require(contains(flow1), s"does not contains $flow1")
    require(contains(flow2), s"does not contains $flow2")
    val connection = YarnConnection(flow1, flow2)
    copy(connections = connections + connection)
  }

  def contains(flow: YarnFlow) =
    flows.get(flow.start).map(_.stream.contains(flow)).getOrElse(false)

  def connectedTo(flow: YarnFlow) =
    connections.flatMap(_.connected(flow))

  private def copy(flows: Map[YarnStart, YarnFlow] = flows, connections: Set[YarnConnection] = connections): Knitted2 = {
    val f2 = flows
    val c2 = connections
    new Knitted2 {
      override val flows = f2
      override val connections = c2
    }
  }

  override def hashCode = flows.hashCode ^ connections.hashCode
  override def equals(o: Any) = o match {
    case o: Knitted2 => flows == o.flows && connections == o.connections
    case _ => false
  }
  override def toString = s"Knitted2(${flows.values.mkString(", ")})"
}

case class YarnConnection(a: YarnFlow, b: YarnFlow) {
  def affects(o: YarnFlow) = a == o || b == o
  def connected(o: YarnFlow): Set[YarnFlow] = {
    if (a == o) Set(b)
    else if (b == o) Set(a)
    else Set.empty
  }
  override def hashCode = a.hashCode ^ b.hashCode
  override def equals(o: Any) = o match {
    case YarnConnection(a2, b2) => (a == a2 && b == b2) || (a == b2 && b == a2)
    case _ => false
  }
}

object Knitted2 {
  object empty extends Knitted2 {
    override val flows = Map.empty[YarnStart, YarnFlow]
    override val connections = Set.empty[YarnConnection]
  }
}