package models.planners

import java.awt.Color
import java.awt.image.BufferedImage
import utils._
import models.{Yarn, Needle}
import models.units.Stitches

object Helper {
  /** First/last needle for the knitting with the given width when center alignment. */
  def center(width: Stitches): (Needle, Needle) = {
    (Needle.middle - (width / 2).approx,
      Needle.middle + (width / 2).approx)
  }

  /** Convert two color image into a yarn pattern. */
  def monochromeToPattern(img: BufferedImage, yarnWhite: Yarn, yarnBlack: Yarn): Matrix[Yarn] = {
    val rgbs = IndexedSeq.tabulate(img.getHeight, img.getWidth)((y, x) => new Color(img.getRGB(x, y)))
    val colors = rgbs.flatten.toSet.toSeq
    require(colors.size <= 2, s"not monochrome: ${colors.toList}")
    val white =
      if (colors.size > 1 && colors(0).getRGB > colors(1).getRGB) colors(1)
      else colors(0)
    rgbs.matrixMap(c => if (c == white) yarnWhite else yarnBlack)
  }

  /** Convert the image into a pattern, yarns are take from colors. */
  def imageToPattern(img: BufferedImage): Matrix[Yarn] = {
    val rgbs = IndexedSeq.tabulate(img.getHeight, img.getWidth)((y, x) => new Color(img.getRGB(x, y)))
    val yarns = colorsToYarns(rgbs.flatten.toSet)
    val pattern = rgbs.matrixMap(yarns).reverseBoth
    pattern
  }

  def colorsToYarns(colors: Set[Color]) = {
    colors.zipWithIndex.map {
      case (c@Color.white, i) => c -> Yarn(s"White", new Color(0xf4f4f4))
      case (c@Color.black, i) => c -> Yarn(s"Black", c)
      case (c@Color.yellow, i) => c -> Yarn(s"Yellow", c)
      case (c@Color.red, i) => c -> Yarn(s"Red", c)
      case (c@Color.green, i) => c -> Yarn(s"Green", c)
      case (c@Color.blue, i) => c -> Yarn(s"Blue", c)
      case (color, i) => color -> Yarn(s"Yarn $i", color)
    }.toMap
  }
}
