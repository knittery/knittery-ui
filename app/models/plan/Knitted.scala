package models.plan

import models._
import utils._

class Knitted private (matrix: Matrix[Stitch]) {
  def data = matrix
  def height = matrix.height
  def width = matrix.width
  
  def rows = matrix.rows

  def +(row: Needle => Stitch) = {
    new Knitted(matrix :+ row.all)
  }

//  def patternString = rows.reverse.mkString("\n")
//  override def toString = patternString
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
}

case class PurlStitch(yarns: List[Yarn]) extends Stitch {
  require(yarns.nonEmpty, "No yarn on purl stitch")
  override def patternString = yarns.head.consoleColor + "-" + Console.RESET
}
object PurlStitch {
  def apply(yarn: Yarn): PurlStitch = PurlStitch(yarn :: Nil)
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