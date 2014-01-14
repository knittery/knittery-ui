package models

sealed trait NeedleAction {
  def toPosition: NeedlePosition
}
case object NeedleToB extends NeedleAction {
  override def toPosition = NeedleB
}
case object NeedleToD extends NeedleAction {
  override def toPosition = NeedleD
}
