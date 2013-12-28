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
case class PlainStich(yarn: Yarn) extends Stich {
  override def patternString = "^"
}
case class PurlStich(yarn: Yarn) extends Stich {
  override def patternString = "-"
}