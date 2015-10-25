package products.laptop

import knit._
import knit.plan._
import knit.planners._
import knit.units._
import squants.space.Length

import scalaz.Scalaz._
import scalaz._

/**
 * Laptop case that wraps the laptop in a rectangular enclosure and has a square lash over the small side.
 */
object Form {
  def apply(width: Length, height: Length, topGap: Length, lash: Length, thickness: Length, patterns: Dimensions => Patterns)
    (implicit gauge: Gauge): Planner = for {
    _ <- Planner.preconditions(_ => true)
    border = 1.stitches //used sew together front and back

    bodyWidth = (width + thickness).toStitches.discardPartials + border * 2
    dims = Dimensions(
      bodyWidth = bodyWidth,
      frontHeight = (height + thickness / 2 - topGap).toRows.discardPartials - 1.rows,
      backHeight = (height + thickness).toRows.discardPartials,
      lashWidth = bodyWidth - ((bodyWidth - width.toStitches) / 2).discardPartials * 2,
      lashHeight = lash.toRows.discardPartials
    )
    sideSize = (thickness.toStitches + border) / 2

    ps <- Planner.preconditions { _ =>
      dims.checkPatterns(patterns(dims))
    }
    first <- Planner.preconditions { _ =>
      require(bodyWidth.approx <= Needle.count - 1)
      Needle.middle - (bodyWidth.approx / 2)
    }
    last = first + bodyWidth.approx - 1
    toDecrease = (bodyWidth - dims.lashWidth).approx / 2
    firstLash = first + toDecrease

    bg <- Cast.onClosed(MainBed, first, last, ps.front.head.head)
    _ <- Basics.knitRowWithK(yarnA = Some(bg))
    _ <- FairIslePlanner.singleBed(ps.front, Some(first))
    _ <- Basics.markLastRow(KnittingMark("front/back"))
    _ <- FairIslePlanner.singleBed(ps.back.reverse, Some(first))

    _ <- Basics.markStitch(KnittingMark("left-side"),
      first + sideSize.approx, MainBed, (dims.frontHeight + dims.backHeight).approx)
    _ <- Basics.markStitch(KnittingMark("right-side"),
      last - sideSize.approx, MainBed, (dims.frontHeight + dims.backHeight).approx)
    hiddenFilter = (n: Needle) =>
      (n >= first && n < (first + border.approx)) || (n <= last && n > last - border.approx)
    _ <- Basics.markStitches(KnittingMark("hidden"),
      hiddenFilter, MainBed, (dims.frontHeight + dims.backHeight).approx)
    _ <- Basics.markLastRow(KnittingMark("back/lash"))

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
}