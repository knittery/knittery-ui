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

case class KnittedRow(stiches: Seq[Stich]) {
  def width = stiches.size
  def patternString = stiches.map(_.patternString).mkString
  override def toString = patternString
}

sealed trait Stich {
  def patternString: String
}
case object NoStich extends Stich {
  override def patternString = " "
}

case class PlainStich(yarns: List[Yarn]) extends Stich {
  require(yarns.nonEmpty, "No yarn on plain stich")
  override def patternString = yarns.head.consoleColor + "^" + Console.RESET
}
object PlainStich {
  def apply(yarn: Yarn): PlainStich = PlainStich(yarn :: Nil)
}

case class PurlStich(yarns: List[Yarn]) extends Stich {
  require(yarns.nonEmpty, "No yarn on purl stich")
  override def patternString = yarns.head.consoleColor + "-" + Console.RESET
}
object PurlStich {
  def apply(yarn: Yarn): PurlStich = PurlStich(yarn :: Nil)
}

case class CastOnStich(yarns: List[Yarn]) extends Stich {
  require(yarns.nonEmpty, "No yarn for cast-on-stich")
  override def patternString = yarns.head.consoleColor + "_" + Console.RESET
}
object CastOnStich {
  def apply(yarn: Yarn): CastOnStich = CastOnStich(yarn :: Nil)
}

case class CastOffStich(yarns: List[Yarn]) extends Stich {
  require(yarns.nonEmpty, "No yarn for cast=off-stich")
  override def patternString = yarns.head.consoleColor + "_" + Console.RESET
}
object CastOffStich {
  def apply(yarn: Yarn): CastOffStich = CastOffStich(yarn :: Nil)
}