import models.NeedlePosition

package object models {
  sealed trait Bed
  case object MainBed extends Bed
  case object DoubleBed extends Bed

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