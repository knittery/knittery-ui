package models.plan

import models._
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import ch.inventsoft.graph.layout.Layout
import ch.inventsoft.graph.vector.Vector3

case class Knitted3D private(ends: Map[YarnPiece, YarnFlow], stitches: Seq[Stitch3D], stitchPosition: Map[Stitch3D, Vector3], yOffset: Int) {
  private def addFlow(ends: Map[YarnPiece, YarnFlow], flow: YarnFlow) = {
    val start = flow.start
    ends.get(start).map { old =>
      if (flow.contains(old)) ends + (start -> flow)
      else if (old.contains(flow)) ends
      else throw new IllegalArgumentException(s"Yarn cannot fork, cannot add $flow, we already contain the different end $old")
    }.getOrElse {
      ends + (start -> flow)
    }
  }

  def add(stitch: Stitch3D, atBed: Bed, atNeedle: Needle) = {
    require(!stitchPosition.contains(stitch), s"Duplicate stitch $stitch")
    val ends2 = stitch.points.foldLeft(ends)(addFlow)
    val pos = Vector3(atNeedle.number, yOffset, if (atBed == MainBed) -1 else 1)
    copy(stitches = stitches :+ stitch,
      stitchPosition = stitchPosition + (stitch -> pos),
      ends = ends2)
  }
  def +(stitch: (Stitch3D, Bed, Needle)) = (add _).tupled(stitch)
  def ++(stitches: Traversable[(Stitch3D, Bed, Needle)]) = stitches.foldLeft(this)(_ + _)

  def pushDown = copy(yOffset = yOffset + 1)

  def contains(flow: YarnFlow) =
    ends.get(flow.start).exists(_.stream.contains(flow))

  def asGraph = {
    val stitchToYarn = stitches.foldLeft(Map.empty[YarnFlow, Stitch3D]) { (map, stitch) =>
      map ++ stitch.points.map(_ -> stitch)
    }

    val edges = ends.values.flatMap { yarn =>
    //reverse through the points of the yarn piece and find the stitches that are
    // connected by this yarn along with their position on the yarn
      val stitches = yarn.stream.map(p => (p, p.length)).flatMap({
        case (point, pos) => stitchToYarn.get(point).map(_ -> pos)
      })
      stitches.sliding(2, 1).collect {
        case (stitchA, posA) #:: (stitchB, posB) #:: _ =>
          val weight = 100 / ((posA - posB).abs + 0.00001d)
          (stitchA ~%+ stitchB)(weight.round.min(1000), yarn.yarn)
      }.toTraversable
    }

    Graph.fromStream(edges = edges)
  }

  def asLayout: Layout[Stitch3D] = stitchPosition

  override def hashCode = ends.hashCode ^ stitches.hashCode
  override def equals(o: Any) = o match {
    case o: Knitted3D => ends == o.ends && stitches == o.stitches
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
case class Stitch3D(left: Set[YarnFlow], right: Set[YarnFlow], noose: Set[YarnFlow]) {
  def points = left ++ right ++ noose
  def affects(p: YarnPoint) = points.contains(p)
}

object Knitted3D {
  val empty = Knitted3D(Map.empty, Seq.empty, Map.empty, 0)
}
