package models

package object plan {
  type NeedleStateRow = Needle => NeedleState

  implicit class RichNeedleStateRow(val row: NeedleStateRow) extends AnyVal {
    def all = Needle.all.map(row)
    def pattern = row.andThen(_.position)
    def yarn = row.andThen(_.yarn)
  }
}