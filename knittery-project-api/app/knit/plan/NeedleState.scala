package knit.plan

import knit._

case class NeedleState(position: NeedlePosition, yarn: Set[YarnFlow]) {
  if (yarn.nonEmpty) require(position.isWorking, "Cannot have yarn on non working needle.")

  override def toString = s"$position(${yarn.mkString(",")})"
}
object NeedleState {
  def apply(position: NeedlePosition): NeedleState =
    NeedleState(position, Set.empty[YarnFlow])
  def apply(position: NeedlePosition, yarn: YarnFlow): NeedleState =
    NeedleState(position, Set(yarn))
  def apply(position: NeedlePosition, yarn: Option[YarnFlow]): NeedleState =
    NeedleState(position, yarn.toSet)
}
