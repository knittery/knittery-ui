package models.plan

import models._

sealed trait Knitted2 {
  val ends: Map[YarnStart, YarnFlow]
  val stitches: Set[Stitch2]

  private def addFlow(ends: Map[YarnStart, YarnFlow], flow: YarnFlow) = {
    val start = flow.start
    ends.get(start).map { old =>
      if (flow.contains(old)) ends + (start -> flow)
      else if (old.contains(flow)) ends
      else throw new IllegalArgumentException(s"Yarn cannot fork, cannot add $flow, we already contain the different end $old")
    }.getOrElse {
      ends + (start -> flow)
    }
  }

  def +(stitch: Stitch2) = {
    val ends2 = stitch.points.foldLeft(ends)(addFlow)
    copy(stitches = stitches + stitch, ends = ends2)
  }

  def contains(flow: YarnFlow) =
    ends.get(flow.start).map(_.stream.contains(flow)).getOrElse(false)

  private def copy(ends: Map[YarnStart, YarnFlow] = ends, stitches: Set[Stitch2] = stitches): Knitted2 = {
    val e2 = ends
    val s2 = stitches
    new Knitted2 {
      override val ends = e2
      override val stitches = s2
    }
  }

  override def hashCode = ends.hashCode ^ stitches.hashCode
  override def equals(o: Any) = o match {
    case o: Knitted2 => ends == o.ends && stitches == o.stitches
    case _ => false
  }
  override def toString = s"Knitted2(${ends.values.mkString(", ")})"
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
case class Stitch2(left: Set[YarnFlow], right: Set[YarnFlow], noose: Set[YarnFlow]) {
  def points = left ++ right ++ noose
  def affects(p: YarnPoint) = points.contains(p)
}

object Knitted2 {
  object empty extends Knitted2 {
    override val ends = Map.empty[YarnStart, YarnFlow]
    override val stitches = Set.empty[Stitch2]
  }
}
