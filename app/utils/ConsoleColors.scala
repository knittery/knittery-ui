package utils

import java.awt.Color

object ConsoleColors {
  val colors = List(Console.RED -> Color.red,
    Console.GREEN -> Color.green,
    Console.YELLOW -> Color.yellow,
    Console.BLUE -> Color.blue,
    Console.MAGENTA -> new Color(255, 0, 255),
    Console.CYAN -> new Color(0, 255, 255),
    Console.WHITE -> Color.white,
    Console.BLACK -> Color.black)

  def closest(color: Color) = {
    colors.sortBy(c => colorDistance(color, c._2)).head._1
  }

  private def colorDistance(c1: Color, c2: Color) = {
    (c1.getRed - c2.getRed) * (c1.getRed - c2.getRed) +
      (c1.getBlue - c2.getBlue) * (c1.getBlue - c2.getBlue) +
      (c1.getGreen - c2.getGreen) * (c1.getGreen - c2.getGreen)
  }
}