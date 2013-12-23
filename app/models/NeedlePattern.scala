package models

import java.awt.image.BufferedImage
import java.awt.Color

/** Knitting position of a needle. */
sealed trait NeedlePosition
/** Needle in A position. */
case object NeedleA extends NeedlePosition
/** Needle in B position. */
case object NeedleB extends NeedlePosition
/** Needle in C position. */
case object NeedleC extends NeedlePosition
/** Needle in D position. */
case object NeedleD extends NeedlePosition

/**
 * Positions of needles.
 */
trait NeedlePattern {
  /** Number of rows in the pattern. */
  def height: Int

  /** Value at the position (pattern starts at 0). */
  def apply(row: Int)(needle: Needle): NeedlePosition
}

object NeedlePattern {
  val empty: NeedlePattern = new NeedlePattern {
    def height = 1
    def apply(row: Int)(needle: Needle) = NeedleA
  }

  def loadCenter(img: BufferedImage) =
    load(img, (Needle.needleCount - img.getWidth()) / 2)
  def load(img: BufferedImage, xOffset: Int = 0): NeedlePattern = new NeedlePattern {
    def height = img.getHeight
    def width = img.getWidth
    def apply(row: Int)(needle: Needle) = {
      require(row >= 0 && row < height, s"Invalid row: $row")
      val x = needle.index - xOffset
      if (x < 0 || x >= width) NeedleA
      else {
        val color = new Color(img.getRGB(width - x - 1, height - row - 1))
        if (color == Color.white) NeedleB
        else NeedleD
      }
    }
  }
}