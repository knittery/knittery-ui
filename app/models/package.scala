import models.NeedlePosition

package object models {
  type NeedlePatternRow = Needle => NeedlePosition
  implicit class RichNeedlePatternRow(val row: NeedlePatternRow) extends AnyVal {
    def all = Needle.all.map(row)
  }

  type NeedleActionRow = Needle => NeedleAction
  implicit class RichNeedleActionRow(val row: NeedleActionRow) extends AnyVal {
    def all = Needle.all.map(row)
  }
}