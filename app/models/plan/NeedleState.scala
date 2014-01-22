package models.plan

import models._

case class NeedleState(position: NeedlePosition, yarn: List[YarnFlow]) {
  if (yarn.nonEmpty) require(position.isWorking, "Cannot have yarn on non working needle.")

  override def toString = s"$position(${yarn.mkString(",")})"
}
object NeedleState {
  def apply(position: NeedlePosition): NeedleState =
    NeedleState(position, Nil)
  def apply(position: NeedlePosition, yarn: YarnFlow): NeedleState =
    NeedleState(position, yarn :: Nil)
  def apply(position: NeedlePosition, yarn: Option[YarnFlow]): NeedleState =
    NeedleState(position, yarn.toList)
}
