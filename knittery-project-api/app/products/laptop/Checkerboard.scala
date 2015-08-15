package products.laptop

import knit._
import knit.units._
import squants.space.Length
import utils._

import scala.util.Random


/** Checkerboard pattern for the laptop case. */
object Checkerboard {

  /** Checkerboard on front and back. It starts at the bottom (front/back are inverted). Lash is plain with yarnA. */
  def apply(yarnA: Yarn, yarnB: Yarn, squareSize: Length)(implicit gauge: Gauge) = {
    val squareWidth = squareSize.toStitches.approx
    val squareHeight = squareSize.toRows.approx

    def checkerboard(w: Int) = {
      val offset = (w % squareWidth) / 2
      val lineA = Stream.continually(Stream.fill(squareWidth)(yarnA) ++ Stream.fill(squareWidth)(yarnB)).flatten
        .drop(offset)
      val lineB = lineA.drop(squareWidth)
      val a = lineA.take(w).toIndexedSeq
      val b = lineB.take(w).toIndexedSeq
      Stream.continually(Stream.fill(squareHeight)(a) ++ Stream.fill(squareHeight)(b)).flatten
    }

    (dims: Dimensions) => {
      val cb = checkerboard(dims.bodyWidth.approx)

      val frontOffset = squareHeight - (dims.frontHeight.approx % squareHeight)
      val front = cb.slice(frontOffset, frontOffset + dims.frontHeight.approx).toIndexedSeq

      val backOffset = squareHeight - (dims.backHeight.approx % squareHeight)
      val back = cb.slice(backOffset, backOffset + dims.backHeight.approx).toIndexedSeq

      val lash = IndexedSeq.fill(dims.lashHeight.approx, dims.lashWidth.approx)(yarnA)
      Patterns(front, back, lash)
    }
  }


  /**
   * Dissolving checkerboard on front and back. Top is dissolved, bottom is checkerboard.
   * Checkerboard starts at the bottom (front/back are reversed). Lash is plain with yarnA.
   */
  def dissolving(yarnA: Yarn, yarnB: Yarn, squareSize: Length, exponent: Double = 1.5, seed: Long = 0)(implicit gauge: Gauge) = {
    val squareWidth = squareSize.toStitches.approx
    val squareHeight = squareSize.toRows.approx
    val random = new Random(seed)

    def dissolve(pattern: Matrix[Yarn], prob: Int => Double, yarn: Yarn) = pattern.zipWithIndex.map {
      case (row, i) =>
        val p = prob(i)
        row.map { y =>
          if (random.nextDouble() < p) yarn
          else y
        }
    }
    def probability(count: Int, offset: Int)(i: Int) = {
      val sector = (i + offset) / squareHeight
      val sectors = count / squareHeight
      Math.pow((sectors - sector - 1).toDouble / sectors, exponent)
    }

    (dims: Dimensions) => {
      val plainPattern = apply(yarnA, yarnB, squareSize)(gauge)(dims)
      val frontOffset = squareHeight - (dims.frontHeight.approx % squareHeight)
      val front = dissolve(plainPattern.front, probability(dims.frontHeight.approx, frontOffset), yarnB)
      val backOffset = squareHeight - (dims.backHeight.approx % squareHeight)
      val back = dissolve(plainPattern.back, probability(dims.frontHeight.approx, backOffset), yarnA)
      Patterns(front, back, plainPattern.lash)
    }
  }
}
