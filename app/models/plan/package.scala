package models

import scalaz._
import Scalaz._

package object plan {
  type NeedleStateRow = Needle => NeedleState

  implicit class RichNeedleStateRow(val row: NeedleStateRow) extends AnyVal {
    def all = Needle.all.map(row)
    def pattern = row.andThen(_.position)
    def yarn = row.andThen(_.yarn)
  }

  implicit def planMonoid: Monoid[KnittingPlan] = new Monoid[KnittingPlan] {
    override def zero = KnittingPlan(Nil)
    override def append(a: KnittingPlan, b: => KnittingPlan) = KnittingPlan(a.steps ++ b.steps)
  }
}