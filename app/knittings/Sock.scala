package knittings

import scalaz._
import Scalaz._
import models._
import models.KCarriage.{DoubleBedCarriage, Settings, HoldingCamH}
import models.plan._
import models.planners.{FormGiving, Basics, Cast}

object Sock {
  /** Width is the diameter of the shaft (half the circumference). */
  def apply(width: Int, shaftHeight: Int, footLength: Int, yarn: YarnPiece) = {
    import KCarriage.{Settings, HoldingCamH, DoubleBedCarriage}
    for {
      _ <- Cast.onClosedRound(Needle.middle - width / 2, Needle.middle + width / 2, yarn)
      _ <- (1 to shaftHeight).toVector.traverse { _ =>
        Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn)
      }
      heelSettings = Settings(holdingCamLever = HoldingCamH)
      heelAssembly = DoubleBedCarriage(partLeft = true, partRight = true)
      _ <- (1 to width / 3 * 2).toVector.traverse { _ =>
        reduceHeel >>
          Basics.knitRowWithK(heelSettings, heelAssembly, Some(yarn))
      }
      _ <- (1 to width / 3 * 2).toVector.traverse { _ =>
        extendHeel >>
          Basics.knitRowWithK(heelSettings, heelAssembly, Some(yarn))
      }
      _ <- (1 to footLength).toVector.traverse { _ =>
        Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn)
      }
      _ <- (1 to (width - 4) / 4).toVector.traverse { _ =>
        FormGiving.raglanDecrease(MainBed, Right) >>
          FormGiving.raglanDecrease(MainBed, Left) >>
          FormGiving.raglanDecrease(DoubleBed, Right) >>
          FormGiving.raglanDecrease(DoubleBed, Left) >>
          Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn) >>
          Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn)
      }
      _ <- (1 to (width - 4) / 4).toVector.traverse { _ =>
        FormGiving.raglanDecrease(MainBed, Right) >>
          FormGiving.raglanDecrease(MainBed, Left) >>
          FormGiving.raglanDecrease(DoubleBed, Right) >>
          FormGiving.raglanDecrease(DoubleBed, Left) >>
          Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn)
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
