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
      transpose.
      //Remove top/bottom empty rows
      dropWhile(emptyOrNo).
      reverse.dropWhile(emptyOrNo).reverse
  }

  def collapse = {
    data.rows.dropWhile(emptyOrNo)
      .reverse.dropWhile(emptyOrNo).reverse
      .foldLeft(Seq.empty[Seq[Stitch]])(collapseFun)
  }
  private def noOverlap(a: Seq[Stitch], b: Seq[Stitch]) = {
    a.zip(b).forall {
      case (NoStitch(_), _) => true
      case (_, NoStitch(_)) => true
      case (EmptyStitch(_), EmptyStitch(_)) => true
      case _ => false
    }
  }
  private def merge(a: Seq[Stitch], b: Seq[Stitch]) = {
    a.zip(b).map {
      case (NoStitch(_), s) => s
      case (s, _) => s
    }
  }
  private def collapseFun(acc: Seq[Seq[Stitch]], b: Seq[Stitch]) = {
    if (acc.isEmpty) Seq(b)
    else if (noOverlap(acc.last, b)) acc.dropRight(1) :+ merge(acc.last, b)
    else acc :+ b
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
  type Self <: Stitch

  def marks: Set[KnittingMark]
  def mark(as: KnittingMark): Self

  def patternString: String
}

case class NoStitch(marks: Set[KnittingMark] = Set.empty) extends Stitch {
  type Self = NoStitch
  override def patternString = "|"
  override def mark(as: KnittingMark) = copy(marks = marks + as)
}

case class EmptyStitch(marks: Set[KnittingMark] = Set.empty) extends Stitch {
  type Self = EmptyStitch
  override def patternString = " "
  override def mark(as: KnittingMark) = copy(marks = marks + as)
}

case class PlainStitch(yarns: List[Yarn], marks: Set[KnittingMark] = Set.empty) extends Stitch {
  require(yarns.nonEmpty, "No yarn on plain stitch")
  type Self = PlainStitch
  override def patternString = yarns.head.consoleColor + "^" + Console.RESET
  override def mark(as: KnittingMark) = copy(marks = marks + as)
}
object PlainStitch {
  def apply(yarn: Yarn): PlainStitch = PlainStitch(yarn :: Nil)
  def orEmpty(yarns: Traversable[Yarn]) = {
    if (yarns.isEmpty) EmptyStitch()
    else apply(yarns.toList)
  }
}

case class PurlStitch(yarns: List[Yarn], marks: Set[KnittingMark] = Set.empty) extends Stitch {
  require(yarns.nonEmpty, "No yarn on purl stitch")
  type Self = PurlStitch
  override def patternString = yarns.head.consoleColor + "-" + Console.RESET
  override def mark(as: KnittingMark) = copy(marks = marks + as)
}
object PurlStitch {
  def apply(yarn: Yarn): PurlStitch = PurlStitch(yarn :: Nil)
  def orEmpty(yarns: Traversable[Yarn]) = {
    if (yarns.isEmpty) EmptyStitch()
    else apply(yarns.toList)
  }
}

case class CastOnStitch(yarns: List[Yarn], marks: Set[KnittingMark] = Set.empty) extends Stitch {
  require(yarns.nonEmpty, "No yarn for cast-on-stitch")
  type Self = CastOnStitch
  override def patternString = yarns.head.consoleColor + "_" + Console.RESET
  override def mark(as: KnittingMark) = copy(marks = marks + as)
}
object CastOnStitch {
  def apply(yarn: Yarn): CastOnStitch = CastOnStitch(yarn :: Nil)
}

case class CastOffStitch(yarns: List[Yarn], marks: Set[KnittingMark] = Set.empty) extends Stitch {
  require(yarns.nonEmpty, "No yarn for cast=off-stitch")
  type Self = CastOffStitch
  override def patternString = yarns.head.consoleColor + "_" + Console.RESET
  override def mark(as: KnittingMark) = copy(marks = marks + as)
}
object CastOffStitch {
  def apply(yarn: Yarn): CastOffStitch = CastOffStitch(yarn :: Nil)
}