import models.NeedlePosition

package object models {
  sealed trait Bed
  case object MainBed extends Bed
  case object DoubleBed extends Bed
  object Beds {
    val all = Set(MainBed, DoubleBed)
  }

  case class Tension(value: Double) extends AnyVal
  implicit class TensionNumeric[A](v: A)(implicit n: Numeric[A]) {
    def tension = Tension(n.toDouble(v))
  }

  type NeedlePatternRow = Needle => NeedlePosition
  implicit class RichNeedlePatternRow(val row: NeedlePatternRow) extends AnyVal {
    def all = Needle.all.map(row)
  }

  type NeedleActionRow = Needle => NeedleAction
  val AllNeedlesToB: NeedleActionRow = _ => NeedleToB
  val AllNeedlesToD: NeedleActionRow = _ => NeedleToD
  implicit class RichNeedleActionRow(val row: NeedleActionRow) extends AnyVal {
    def invert: NeedleActionRow = n => row(n) match {
      case NeedleToB => NeedleToD
      case NeedleToD => NeedleToB
    }
  }

  implicit class RichNeedleFun[A](val f: Needle => A) extends AnyVal {
    def all = Needle.all.map(f)
  }
}