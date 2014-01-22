package models.plan

import models._

sealed trait Knitted2 {
  val flows: Map[YarnStart, YarnFlow]
  val stitches: Set[Stitch2]

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

  def +(stitch: Stitch2) = {
    require(stitch.points.forall(contains), s"missing yarn flows in stitch $stitch")
    copy(stitches = stitches + stitch)
  }

  def contains(flow: YarnFlow) =
    flows.get(flow.start).map(_.stream.contains(flow)).getOrElse(false)

  private def copy(flows: Map[YarnStart, YarnFlow] = flows, stitches: Set[Stitch2] = stitches): Knitted2 = {
    val f2 = flows
    val s2 = stitches
    new Knitted2 {
      override val flows = f2
      override val stitches = s2
    }
  }

  override def hashCode = flows.hashCode ^ stitches.hashCode
  override def equals(o: Any) = o match {
    case o: Knitted2 => flows == o.flows && stitches == o.stitches
    case _ => false
  }
  override def toString = s"Knitted2(${flows.values.mkString(", ")})"
}

/**
 * A Stitch connects three YarnPoints - left, right and noose.
 * Left and right belong to the same yarn and form a new noose. The noose is abstracted
 * to a single YarnPoint.
 * If looked at it from the front (so left+right are properly named) then the noose lies
 * BEHIND the new noose (left thread first goes behind and then in front of the noose yarn).
 * So it forms the popular knitted V's in the front and the lying-S in the back.
 * See a schematic picture of a knitted garment to understand this stuff better :).
 */
case class Stitch2(left: YarnPoint, right: YarnPoint, noose: YarnPoint) {
  def points = left :: right :: noose :: Nil
  def affects(p: YarnPoint) = points.contains(p)
}

object Knitted2 {
  object empty extends Knitted2 {
    override val flows = Map.empty[YarnStart, YarnFlow]
    override val stitches = Set.empty[Stitch2]
  }
}
