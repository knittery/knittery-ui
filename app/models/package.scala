import models.NeedlePosition

package object models {
  type NeedlePatternRow = Needle => NeedlePosition

  implicit class RichNeedlePatternRow(val row: NeedlePatternRow) extends AnyVal {
    def all = Needle.all.map(row)
  }
}