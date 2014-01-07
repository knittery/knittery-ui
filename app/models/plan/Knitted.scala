package models.plan

case class Knitted(rows: Seq[KnittedRow]) {
  if (rows.size > 0) require(rows.forall(_.width == rows(0).width), "Non-uniform width")

  def height = rows.size
  def width = {
    if (rows.size == 0) throw new IllegalStateException("Unknown width.")
    rows(0).width
  }

  def +(row: KnittedRow) = copy(rows :+ row)

  def patternString = rows.reverse.mkString("\n")
  override def toString = patternString
}
object Knitted {
  def empty = Knitted(Seq.empty)
}

case class KnittedRow(stitches: Seq[Stitch]) {
  def width = stitches.size
  def patternString = stitches.map(_.patternString).mkString
  override def toString = patternString
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