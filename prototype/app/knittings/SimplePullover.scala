package knittings

import models.planners.{Basics, Cast, FormGiving}

import scalaz._
import Scalaz._
import models._
import models.plan.{MoveNeedles, Planner}
import models.units._

/** Simplified pullover. */
object SimplePullover {

  def arm(yarn: Yarn)(implicit gauge: Gauge): Planner = for {
    _ <- Planner.noop
    base = 16.stitches
    armLength = 40.rows
    armholeWidth = 24.stitches
    armholeHeight = 10.rows
    //    armholeTop = 4.stitches
    armholeTop = 10.stitches

    firstNeedle = Needle.middle - (base / 2).approx
    yarnPiece = YarnPiece(yarn)

    contrast <- Cast.onOpen(firstNeedle, firstNeedle + base.approx - 1, YarnPiece(Yarn.contrastYarn))
    _ <- (1 to 10).toVector.traverse(_ => Basics.knitRowWithK(yarnA = Some(contrast)))

    _ <- knitRows(armLength, (base - armholeWidth) / 2, (base - armholeWidth) / 2)(yarnPiece)

    _ <- Basics.knitRowWithK(yarnA = Some(yarnPiece))

    _ <- knitRows(armholeHeight, (armholeWidth - armholeTop) / 2, (armholeWidth - armholeTop) / 2)(yarnPiece)
  } yield ()

  def knitRows(count: Rows, leftReduce: Stitches, rightReduce: Stitches)(yarn: YarnPiece) = for {
    leftmostNeedle <- Planner.state(_.workingNeedles(MainBed).head)
    rightmostNeedle <- Planner.state(_.workingNeedles(MainBed).last)
    lefts = rowsWithDecreases(count, leftReduce).map(leftmostNeedle + _.approx)
    rights = rowsWithDecreases(count, rightReduce).map(rightmostNeedle - _.approx)
    _ <- lefts.zip(rights).toVector.traverse {
      case (newLeft, newRight) =>
        for {
          currentLeft <- Planner.state(_.workingNeedles(MainBed).head)
          toReduceLeft = newLeft.index - currentLeft.index
          currentRight <- Planner.state(_.workingNeedles(MainBed).last)
          toReduceRight = currentRight.index - newRight.index
          _ <- knitRowWithReduceOrIncrease(toReduceLeft, toReduceRight)(yarn)
        } yield ()
    }
  } yield ()

  def rowsWithDecreases(count: Rows, toReduce: Stitches): Seq[Stitches] = {
    (1 to count.approx).map { row =>
      val x = (toReduce.amount.abs + 1) * row.toDouble / (count.amount + 1)
      (x.floor * toReduce.amount.signum).stitches
    }
  }

  def knitRowWithReduceOrIncrease(leftReduce: Int, rightReduce: Int)(yarn: YarnPiece) = {
    val left = if (leftReduce > 0) {
      (0 until leftReduce).toVector.traverse(_ => FormGiving.raglanWithLCarriage(3, true, false))
    } else if (leftReduce < 0) {
      for {
        needlesBefore <- Planner.state(_.needles(MainBed))
        leftmostNeedle <- Planner.state(_.workingNeedles(MainBed).head)
        needlesToB = (1 to -leftReduce).map(i => leftmostNeedle - i)
        nsr = needlesToB.map(_ -> NeedleB).toMap.withDefault(n => needlesBefore(n).position)
        _ <- MoveNeedles(MainBed, nsr)
      } yield ()
    } else Planner.noop

    val right = if (rightReduce > 0) {
      (0 until rightReduce).toVector.traverse(_ => FormGiving.raglanWithLCarriage(3, false, true))
    } else if (rightReduce < 0) {
      for {
        needlesBefore <- Planner.state(_.needles(MainBed))
        rightmostNeedle <- Planner.state(_.workingNeedles(MainBed).last)
        needlesToB = (1 to -rightReduce).map(i => rightmostNeedle + i)
        nsr = needlesToB.map(_ -> NeedleB).toMap.withDefault(n => needlesBefore(n).position)
        _ <- MoveNeedles(MainBed, nsr)
      } yield ()
    } else Planner.noop

    left >> right >> Basics.knitRowWithK(yarnA = Some(yarn))
  }
}
