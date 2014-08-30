package knittings

import scala.util.Random
import scalaz._
import Scalaz._
import squants.space.Length
import utils._
import models._
import models.plan._
import models.units._
import models.planners.{FormGiving, FairIslePlanner, Basics, Cast}

/**
 * Laptop case that wraps the laptop in a rectangular enclosure and has a square lash over the small side.
 */
object LaptopCase {
  case class Patterns(front: Matrix[Yarn], back: Matrix[Yarn], lash: Matrix[Yarn])
  case class Dimensions(bodyWidth: Stitches, frontHeight: Rows, backHeight: Rows, lashWidth: Stitches, lashHeight: Rows) {
    def checkPatterns(ps: Patterns) = {
      require(ps.front.size == frontHeight.approx, "wrong front height")
      require(ps.front.head.size == bodyWidth.approx, "wrong front width")
      require(ps.back.size == backHeight.approx, "wrong back height")
      require(ps.back.head.size == bodyWidth.approx, "wrong back width")
      require(ps.lash.size == lashHeight.approx, "wrong lash height")
      require(ps.lash.head.size == lashWidth.approx, "wrong lash width")
      ps
    }
  }

  def form(width: Length, height: Length, topGap: Length, lash: Length, thickness: Length, patterns: Dimensions => Patterns)
    (implicit gauge: Gauge): Planner = for {
    _ <- Planner.precondidtions(_ => true)
    border = 2.stitches //used sew together front and back

    bodyWidth = gauge.stitchesFor(width + thickness).discardPartials + border
    dims = Dimensions(
      bodyWidth = bodyWidth,
      frontHeight = gauge.rowsFor(height + thickness / 2 - topGap).discardPartials - 1.rows,
      backHeight = gauge.rowsFor(height + thickness).discardPartials,
      lashWidth = bodyWidth - ((bodyWidth - gauge.stitchesFor(width)) / 2).discardPartials * 2,
      lashHeight = gauge.rowsFor(lash).discardPartials
    )

    ps <- Planner.precondidtions { _ =>
      dims.checkPatterns(patterns(dims))
    }
    first <- Planner.precondidtions { _ =>
      require(bodyWidth.approx <= Needle.count - 1)
      Needle.middle - (bodyWidth.approx / 2)
    }
    last = first + bodyWidth.approx - 1
    toDecrease = (bodyWidth - dims.lashWidth).approx / 2
    firstLash = first + toDecrease


    bg <- Cast.onClosed(MainBed, first, last, ps.front(0)(0))
    _ <- Basics.knitRowWithK(yarnA = Some(bg))
    _ <- FairIslePlanner.singleBed(ps.front, Some(first))
    _ <- FairIslePlanner.singleBed(ps.back.reverse, Some(first))
    _ <- FairIslePlanner.singleBed(ps.lash.take(1), Some(firstLash))
    _ <- (1 to toDecrease).toVector.traverse { i =>
      FormGiving.raglanDecrease(MainBed, Left) >>
        FormGiving.raglanDecrease(MainBed, Right) >>
        FairIslePlanner.singleBed(ps.lash.drop(i).take(1), Some(firstLash))
    }
    _ <- FairIslePlanner.singleBed(ps.lash.drop(toDecrease + 1), Some(firstLash))
    _ <- Cast.offClosed(MainBed, bg)
  } yield ()


  /** Checkerboard on front and back. It starts at the bottom (front/back are inversed). Lash is plain with yarnA. */
  def checkerboardPattern(yarnA: Yarn, yarnB: Yarn, squareSize: Length)(implicit gauge: Gauge) = {
    val squareWidth = gauge.stitchesFor(squareSize).approx
    val squareHeight = gauge.rowsFor(squareSize).approx

    def checkerboard(w: Int) = {
      val offset = (w % squareWidth) / 2
      val lineA = Stream.continually(Stream.fill(squareWidth)(yarnA) ++ Stream.fill(squareWidth)(yarnB)).flatten.drop(offset)
      val lineB = lineA.drop(squareWidth)
      val a = lineA.take(w).toIndexedSeq
      val b = lineB.take(w).toIndexedSeq
      Stream.continually(Stream.fill(squareHeight)(a) ++ Stream.fill(squareHeight)(b)).flatten
    }

    (dims: Dimensions) => {
      val cb = checkerboard(dims.bodyWidth.approx)

      val frontOffset = squareHeight - (dims.frontHeight.approx % squareHeight)
      val front = cb.drop(frontOffset).take(dims.frontHeight.approx).toIndexedSeq

      val backOffset = squareHeight - (dims.backHeight.approx % squareHeight)
      val back = cb.drop(backOffset).take(dims.backHeight.approx).toIndexedSeq

      val lash = IndexedSeq.fill(dims.lashHeight.approx, dims.lashWidth.approx)(yarnA)
      Patterns(front, back, lash)
    }
  }

  /** Dissolving checkerboard on front and back. Top is dissolved, bottom is checkerboard.
    * Checkerboard starts at the bottom (front/back are inversed). Lash is plain with yarnA. */
  def dissolvingCheckerboardPattern(yarnA: Yarn, yarnB: Yarn, squareSize: Length, exponent: Double = 1.5, seed: Long = 0)(implicit gauge: Gauge) = {
    val squareWidth = gauge.stitchesFor(squareSize).approx
    val squareHeight = gauge.rowsFor(squareSize).approx
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
      val plainPattern = checkerboardPattern(yarnA, yarnB, squareSize)(gauge)(dims)
      val frontOffset = squareHeight - (dims.frontHeight.approx % squareHeight)
      val front = dissolve(plainPattern.front, probability(dims.frontHeight.approx, frontOffset), yarnB)
      val backOffset = squareHeight - (dims.backHeight.approx % squareHeight)
      val back = dissolve(plainPattern.back, probability(dims.frontHeight.approx, backOffset), yarnA)
      Patterns(front, back, plainPattern.lash)
    }
  }
}
