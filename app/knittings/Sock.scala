package knittings

import scalaz._
import Scalaz._
import squants.space.Length
import squants.space.LengthConversions._
import models._
import models.KCarriage.{DoubleBedCarriage, Settings, HoldingCamH}
import models.plan._
import models.planners.{FormGiving, Basics, Cast}
import models.units._

object Sock {
  /** Knit socks according to eu-foot size. */
  def europeanSize(euSize: Int, yarn: Yarn, shaftRelativeToFoot: Double = 1.3)(implicit gauge: Gauge): Planner = {
    val (circumference, rawFootLength) = euSize match {
      case 34 => (56.stitches, 22.0 cm)
      case 35 => (58.stitches, 22.7 cm)
      case 36 => (58.stitches, 23.0 cm)
      case 37 => (60.stitches, 23.8 cm)
      case 38 => (60.stitches, 24.3 cm)
      case 39 => (62.stitches, 25.1 cm)
      case 40 => (62.stitches, 25.6 cm)
      case 41 => (64.stitches, 26.5 cm)
      case 42 => (64.stitches, 27.0 cm)
      case 43 => (66.stitches, 27.8 cm)
      case 44 => (68.stitches, 28.3 cm)
      case 45 => (68.stitches, 29.1 cm)
      case 46 => (72.stitches, 29.6 cm)
      case 47 => (72.stitches, 30.3 cm)
    }
    val footLength = rawFootLength * 0.63
    val shaftHeight = (footLength * shaftRelativeToFoot).toRows
    val heelLength = (circumference.toLength / 2 / Math.PI)
    Sock(circumference / 2, shaftHeight, (footLength - heelLength).toRows, yarn)
  }

  /** Circumference relates to the shaft (foot will be the same). */
  def apply(circumference: Length, shaftHeight: Length, footLength: Length, yarn: Yarn)(implicit gauge: Gauge): Planner = {
    Sock(circumference.toStitches / 2, shaftHeight.toRows, (footLength - circumference / 2 / Math.PI).toRows, yarn)
  }

  /** Width is the diameter of the shaft (half the circumference). */
  def apply(diameter: Stitches, shaftHeight: Rows, footLengthFromHeel: Rows, yarn: Yarn) = {
    import KCarriage.{Settings, HoldingCamH, DoubleBedCarriage}
    val width = diameter.approx
    for {
      yarnPiece <- Cast.onClosedRound(Needle.middle - width / 2, Needle.middle + width / 2, YarnPiece(yarn))
      _ <- (1 to shaftHeight.approx).toVector.traverse { _ =>
        Basics.knitRoundK(yarnPiece) >> Basics.knitRoundK(yarnPiece)
      }
      heelSettings = Settings(holdingCamLever = HoldingCamH)
      heelAssembly = DoubleBedCarriage(partLeft = true, partRight = true)
      _ <- (1 to width / 3 * 2).toVector.traverse { _ =>
        reduceHeel >>
          Basics.knitRowWithK(heelSettings, heelAssembly, Some(yarnPiece))
      }
      _ <- (1 to width / 3 * 2).toVector.traverse { _ =>
        extendHeel >>
          Basics.knitRowWithK(heelSettings, heelAssembly, Some(yarnPiece))
      }
      _ <- (1 to footLengthFromHeel.approx).toVector.traverse { _ =>
        Basics.knitRoundK(yarnPiece) >> Basics.knitRoundK(yarnPiece)
      }
      _ <- (1 to (width.rows.approx - 4) / 4).toVector.traverse { _ =>
        FormGiving.raglanDecrease(MainBed, Right) >>
          FormGiving.raglanDecrease(MainBed, Left) >>
          FormGiving.raglanDecrease(DoubleBed, Right) >>
          FormGiving.raglanDecrease(DoubleBed, Left) >>
          Basics.knitRoundK(yarnPiece) >> Basics.knitRoundK(yarnPiece) >>
          Basics.knitRoundK(yarnPiece) >> Basics.knitRoundK(yarnPiece)
      }
      _ <- (1 to width.rows.approx / 4).toVector.traverse { _ =>
        FormGiving.raglanDecrease(MainBed, Right) >>
          FormGiving.raglanDecrease(MainBed, Left) >>
          FormGiving.raglanDecrease(DoubleBed, Right) >>
          FormGiving.raglanDecrease(DoubleBed, Left) >>
          Basics.knitRoundK(yarnPiece) >> Basics.knitRoundK(yarnPiece)
      }
    } yield ()
  }

  private def reduceHeel() = for {
    working <- Planner.state(s =>
      Needle.all.filter { n =>
        val pos = s.needles(MainBed)(n).position
        pos.isWorking && pos != NeedleE
      })
    dir <- Planner.validate(_.nextDirection(KCarriage))
    needle = if (dir == ToLeft) working.last else working.head
    needles <- Planner.state(_.needles(MainBed))
    _ <- Basics.moveNeedles(MainBed, _ == needle, NeedleE)
  } yield ()

  private def extendHeel() = for {
    working <- Planner.state(s =>
      Needle.all.filter { n =>
        val pos = s.needles(MainBed)(n).position
        pos.isWorking && pos != NeedleE
      })
    dir <- Planner.validate(_.nextDirection(KCarriage))
    needle = if (dir == ToLeft) working.last + 1 else working.head - 1
    needles <- Planner.state(_.needles(MainBed))
    _ <- Basics.moveNeedles(MainBed, _ == needle, NeedleD)
  } yield ()
}
