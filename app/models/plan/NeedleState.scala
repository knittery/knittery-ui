package models.plan

import models.NeedlePosition

case class NeedleState(position: NeedlePosition, yarn: List[Yarn]) {
  if (yarn.nonEmpty) require(position.isWorking, "Cannot have yarn on non working needle.")

  override def toString = s"$position(${yarn.mkString(",")})"
}
object NeedleState {
  def apply(position: NeedlePosition): NeedleState =
    NeedleState(position, Nil)
  def apply(position: NeedlePosition, yarn: Yarn): NeedleState =
    NeedleState(position, yarn :: Nil)
  def apply(position: NeedlePosition, yarn: Option[Yarn]): NeedleState =
    NeedleState(position, yarn.toList)
}
