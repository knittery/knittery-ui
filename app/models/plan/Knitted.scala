package models.plan

import models._
import utils._

case class Knitted private (matrix: Matrix[Stitch]) {
  def data = matrix
  def height = matrix.height
  def width = matrix.width

  def rows = matrix.rows

  def +(row: Needle => Stitch) = new Knitted(matrix :+ row.all)

  private def emptyOrNo(s: Seq[Stitch]) = s.forall(s => s == NoStitch || s == EmptyStitch)
  def clean = {
    matrix.rows.
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
object Knitted {
  def empty = new Knitted(IndexedSeq.empty)
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