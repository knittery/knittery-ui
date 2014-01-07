import models.NeedlePosition

package object models {
  type NeedlePatternRow = Needle => NeedlePosition
  implicit class RichNeedlePatternRow(val row: NeedlePatternRow) extends AnyVal {
    def all = Needle.all.map(row)
  }

  type NeedleActionRow = Needle => NeedleAction

  implicit class RichNeedleFun[A](val f: Needle => A) extends AnyVal {
    def all = Needle.all.map(f)
  }
}