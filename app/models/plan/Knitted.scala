package models.plan

import models._
import utils._

case class Knitted private(mainBed: KnittedBed, doubleBed: KnittedBed) {
  def height = mainBed.height
  def width = mainBed.width

  def +(mainBedRow: Needle => Stitch, doubleBedRow: Needle => Stitch) =
    new Knitted(mainBed + mainBedRow, doubleBed + doubleBedRow)
}
object Knitted {
  val empty = new Knitted(KnittedBed.empty, KnittedBed.empty)
}

case class KnittedBed private(data: Matrix[Stitch]) {
  def height = data.height
  def width = data.width

  def rows = data.rows

  def +(row: Needle => Stitch) = new KnittedBed(data :+ row.all)

  private def emptyOrNo(s: Seq[Stitch]) = s.forall(s => s == NoStitch || s == EmptyStitch)
  def clean = {
    data.rows.
      filterNot(_.forall(_ == NoStitch)). // Filter all rows containing no stitches
      transpose.
      // Remove empty columns at the side
      dropWhile(emptyOrNo).
      reverse.dropWhile(emptyOrNo).reverse.
      map { column =>
      column
    }.
      transpose.
      //Remove top/bottom empty rows
      dropWhile(emptyOrNo).
      reverse.dropWhile(emptyOrNo).reverse
  }

  def patternString = {
    clean.reverse.
      map(_.map(_.patternString).mkString).
      mkString("\n")
  }

  override def toString = patternString
}
object KnittedBed {
  val empty = new KnittedBed(IndexedSeq.empty)
}

sealed trait Stitch {
  def patternString: String
}

case object NoStitch extends Stitch {
  override def patternString = "|"
}

case object EmptyStitch extends Stitch {
  override def patternString = " "
}

case class PlainStitch(yarns: List[Yarn]) extends Stitch {
  require(yarns.nonEmpty, "No yarn on plain stitch")
  override def patternString = yarns.head.consoleColor + "^" + Console.RESET
}
object PlainStitch {
  def apply(yarn: Yarn): PlainStitch = PlainStitch(yarn :: Nil)
  def orEmpty(yarns: Traversable[Yarn]) = {
    if (yarns.isEmpty) EmptyStitch
    else apply(yarns.toList)
  }
}

case class PurlStitch(yarns: List[Yarn]) extends Stitch {
  require(yarns.nonEmpty, "No yarn on purl stitch")
  override def patternString = yarns.head.consoleColor + "-" + Console.RESET
}
object PurlStitch {
  def apply(yarn: Yarn): PurlStitch = PurlStitch(yarn :: Nil)
  def orEmpty(yarns: Traversable[Yarn]) = {
    if (yarns.isEmpty) EmptyStitch
    else apply(yarns.toList)
  }
}

case class CastOnStitch(yarns: List[Yarn]) extends Stitch {
  require(yarns.nonEmpty, "No yarn for cast-on-stitch")
  override def patternString = yarns.head.consoleColor + "_" + Console.RESET
}
object CastOnStitch {
  def apply(yarn: Yarn): CastOnStitch = CastOnStitch(yarn :: Nil)
}

case class CastOffStitch(yarns: List[Yarn]) extends Stitch {
  require(yarns.nonEmpty, "No yarn for cast=off-stitch")
  override def patternString = yarns.head.consoleColor + "_" + Console.RESET
}
object CastOffStitch {
  def apply(yarn: Yarn): CastOffStitch = CastOffStitch(yarn :: Nil)
}