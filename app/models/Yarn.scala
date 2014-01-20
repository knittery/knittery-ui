package models

import java.awt.Color
import utils.ConsoleColors

case class Yarn(name: String, color: Color) {
  def consoleColor = ConsoleColors.closest(color)
  override def toString = name
}

sealed trait YarnD {
  def yarn: Yarn
  def next = YarnPoint(yarn, this)
  def previous: Option[YarnD]
}
case class YarnPoint(yarn: Yarn, prev: YarnD) extends YarnD {
  override def previous = Some(prev)
}
case class YarnStart(yarn: Yarn) extends YarnD {
  override def previous = None
}