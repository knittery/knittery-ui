package models.plan

import models.NeedlePosition

case class NeedleState(position: NeedlePosition, yarn: Option[Yarn] = None) {
  override def toString = s"$position(${yarn.getOrElse("_")})"
}
