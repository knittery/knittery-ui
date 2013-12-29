package models.plan

import java.awt.Color
import utils.ConsoleColors

case class Yarn(name: String, color: Color) {
  def consoleColor = ConsoleColors.closest(color)
  override def toString = name
}