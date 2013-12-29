package models.plan

import models.NeedlePosition

case class NeedleState(position: NeedlePosition, yarn: Option[Yarn] = None) {
  if (yarn.isDefined) require(position.isWorking, "Cannot have yarn on non working needle.")

  override def toString = s"$position(${yarn.getOrElse("_")})"
}
