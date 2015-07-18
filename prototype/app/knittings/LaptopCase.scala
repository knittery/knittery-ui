package knittings

import java.io.File
import javax.imageio.ImageIO
import scala.util.Random
import scalaz._
import Scalaz._
import squants.space.Length
import utils._
import models._
import models.plan._
import models.units._
import models.planners._

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
      require(ps.lash.size == lashHeight.approx, s"wrong lash height: ${ps.lash.size} instead of ${lashHeight.approx}")
      require(ps.lash.head.size == lashWidth.approx, s"wrong lash width: ${ps.lash.head.size} instead of ${lashWidth.approx}")
      ps
    }
  }

  def form(width: Length, height: Length, topGap: Length, lash: Length, thickness: Length, patterns: Dimensions => Patterns)
    (implicit gauge: Gauge): Planner = for {
    _ <- Planner.precondidtions(_ => true)
    border = 2.stitches //used sew together front and back

    bodyWidth = (width + thickness).toStitches.discardPartials + border
    dims = Dimensions(
      bodyWidth = bodyWidth,
      frontHeight = (height + thickness / 2 - topGap).toRows.discardPartials - 1.rows,
      backHeight = (height + thickness).toRows.discardPartials,
      lashWidth = bodyWidth - ((bodyWidth - width.toStitches) / 2).discardPartials * 2,
      lashHeight = lash.toRows.discardPartials
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
    _ <- (0 until toDecrease).toVector.traverse { i =>
      for {
        working <- Planner.state(_.workingNeedles(MainBed).size)
        patternLine = ps.lash.drop(i).head
        delta = (working - patternLine.size) / 2
        line = IndexedSeq.fill(delta)(patternLine.head) ++ patternLine ++ IndexedSeq.fill(delta)(patternLine.last)
        _ <- FairIslePlanner.singleBed(IndexedSeq(line), Some(firstLash - delta))
        _ <- FormGiving.raglanDecrease(MainBed, Left)
        _ <- FormGiving.raglanDecrease(MainBed, Right)
      } yield ()
    }
    _ <- FairIslePlanner.singleBed(ps.lash.drop(toDecrease), Some(firstLash))
    _ <- Cast.offClosed(MainBed, bg)
  } yield ()


  /** Checkerboard on front and back. It starts at the bottom (front/back are inverted). Lash is plain with yarnA. */
  def checkerboardPattern(yarnA: Yarn, yarnB: Yarn, squareSize: Length)(implicit gauge: Gauge) = {
    val squareWidth = squareSize.toStitches.approx
    val squareHeight = squareSize.toRows.approx

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
      val plainPattern = checkerboardPattern(yarnA, yarnB, squareSize)(gauge)(dims)
      val frontOffset = squareHeight - (dims.frontHeight.approx % squareHeight)
      val front = dissolve(plainPattern.front, probability(dims.frontHeight.approx, frontOffset), yarnB)
      val backOffset = squareHeight - (dims.backHeight.approx % squareHeight)
      val back = dissolve(plainPattern.back, probability(dims.frontHeight.approx, backOffset), yarnA)
      Patterns(front, back, plainPattern.lash)
    }
  }

  def gradient(yarnTop: Yarn, yarnBottom: Yarn, yarnForLash: Yarn, seed: Long = 0) = (dims: Dimensions) => {
    val random = new Random(seed)
    val border = 2.stitches

    def gradientPattern(width: Int, height: Int, top: Yarn, bottom: Yarn) = {
      (0 until height).map(_.toDouble / height).map { prob =>
        IndexedSeq.fill(width)(if (random.nextDouble() < prob) bottom else top)
      }
    }
    def addBorder(to: Matrix[Yarn]) = {
      val b = IndexedSeq.fill(border.approx)(yarnTop)
      to.map(b ++ _ ++ b)
    }

    val width = dims.bodyWidth - border * 2
    val front = addBorder(gradientPattern(width.approx, dims.frontHeight.approx, yarnTop, yarnBottom))
    val back = addBorder(gradientPattern(width.approx, dims.backHeight.approx, yarnTop, yarnBottom))
    val lash = IndexedSeq.fill(dims.lashHeight.approx, dims.lashWidth.approx)(yarnForLash)
    Patterns(front, back, lash)
  }

  def diamond(yarnA: Yarn, yarnB: Yarn, xOffset: Int = 0, yOffset: Int = 0) = (dims: Dimensions) => {
    val fh = dims.frontHeight.approx

    val diamondImg = ImageIO.read(new File("pattern/muster_raute.png"))
    val diamond = Helper.monochromeToPattern(diamondImg, yarnA, yarnB).
      tile(dims.bodyWidth.approx, fh + dims.backHeight.approx, xOffset, yOffset)

    val lash = IndexedSeq.tabulate(dims.lashHeight.approx, dims.lashWidth.approx) { (c, r) =>
      if (c % 2 == r % 2) yarnA
      else yarnB
    }

    Patterns(diamond.take(fh), diamond.drop(fh), lash)
  }
}
