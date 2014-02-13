package models.plan.knitting

import scala.util.Try
import models._
import models.KCarriage._
import models.plan._
import utils._

class KKnitting(carriageState: State, state: KnittingState, direction: Direction) {
  def apply(pattern: NeedleActionRow) = Try {
    val main = mainBed(pattern)
    val double = doubleBed.getOrElse(noop _)
    val initial = ResultBuilder(carriageState.yarnA, carriageState.yarnB, state.yarnAttachments)
    val needles = direction match {
      case ToRight => Needle.all
      case ToLeft => Needle.all.reverse
    }
    needles.foldLeft(initial) { (x, n) =>
      val x2 = main(x, n)
      double(x2, n)
    }.execute(state)
  }.toSuccess

  private def settings = carriageState.settings

  private def mainBed(pattern: NeedleActionRow) = {
    val bed = new KMainBed(settings.holdingCamLever == HoldingCamN, pattern, state.needles)
    val part = settings.part(direction)
    val tuck = settings.tuck(direction)
    (settings.mc, settings.l, part, tuck, carriageState.yarnA, carriageState.yarnB) match {
      case (false, false, false, false, Some(yarn), None) =>
        bed.plain _
      case (false, false, true, false, Some(yarn), None) =>
        bed.part _
      case (false, false, false, true, Some(yarn), None) =>
        ??? //tuck
      case (true, false, false, false, Some(yarnA), Some(yarnB)) =>
        bed.mc _
      case (_, true, false, false, Some(yarn), Some(yarnB)) =>
        ??? //l
      case (_, _, false, false, None, None) =>
        ??? //throw yarn
      case (false, false, _, _, None, None) =>
        ??? //throw yarn on D's
      case _ =>
        throw new IllegalStateException(s"Illegal settings for K (main bed): $carriageState")
    }
  }

  private def doubleBed = carriageState.assembly match {
    case sp: SinkerPlate => None
    case dbc: DoubleBedCarriage => Some {
      require(!settings.mc, "MC not supported with double bed assembly.")
      require(!settings.l, "L not supported with double bed assembly.")
      val bed = new KDoubleBed(dbc.needleTakeback(direction), state.doubleBedNeedles)
      val part = dbc.part(direction)
      (part) match {
        case (false) => bed.plain _
        case (true) => bed.part _
      }
    }
  }

  private def noop(x: ResultBuilder, n: Needle) = x
}

/** Knitting on the main bed of with the K-carriage. */
private class KMainBed(takeback: Boolean, pattern: NeedleActionRow, needles: NeedleStateRow) {
  private implicit def toSet[A](a: A): Set[A] = Set(a)

  def plain(x: ResultBuilder, n: Needle): ResultBuilder = (x, (n, needles(n).position, needles(n).yarn)) match {
    case (x, (_, NeedleA, _)) =>
      //don't knit A needles
      x
    case (x, (n, NeedleE, ys)) if !takeback =>
      //don't knit E needles if no needle pull back from E
      //TODO do we need to "prevent" falling down of yarn in the yarn feeder
      x.needle(n, NeedleE, ys).knit(n, NoStitch)
    case (x, (n, _, ys)) =>
      //knit normally
      val (x2, (l, t, r)) = x.withYarnA(_.to(n, true).noose)
      x2.knit(Stitch2(l, ys, r)).
        knit(n, PlainStitch.orEmpty(ys.map(_.yarn))).
        needle(n, pattern(n).toPosition, t)
  }

  def part(x: ResultBuilder, n: Needle): ResultBuilder = (x, (n, needles(n).position, needles(n).yarn)) match {
    case (x, (_, NeedleA, _)) =>
      //don't knit A needles
      x
    case (x, (n, NeedleE, ys)) if !takeback =>
      //don't knit E needles if no needle pull back from E
      //TODO do we need to "prevent" falling down of yarn in the yarn feeder
      x.needle(n, NeedleE, ys).knit(n, NoStitch)
    case (x, (n, NeedleB, ys)) =>
      // don't knit B needles with part
      x.needle(n, pattern(n).toPosition, ys).
        knit(n, NoStitch)
    case (x, (n, _, ys)) =>
      //knit normally
      val (x2, noose) = x.withYarnA(_.to(n, true).noose)
      x2.knit(Stitch2(noose._1, noose._3, ys)).
        knit(n, PlainStitch(ys.map(_.yarn).toList)).
        needle(n, pattern(n).toPosition, noose._2)
  }

  def mc(x: ResultBuilder, n: Needle): ResultBuilder = (x, (n, needles(n).position, needles(n).yarn)) match {
    case (x, (_, NeedleA, _)) =>
      //don't knit A needles
      x
    case (x, (n, NeedleE, ys)) if !takeback =>
      //don't knit E needles if no needle pull back from E
      //TODO do we need to "prevent" falling down of yarn in the yarn feeder
      x.needle(n, NeedleE, ys).knit(n, NoStitch)
    case (x, (n, NeedleB, ys)) =>
      //knit yarnA
      val (x2, noose) = x.withYarnA(_.to(n, true).noose)
      x2.knit(Stitch2(noose._1, noose._3, ys)).
        knit(n, PlainStitch.orEmpty(ys.map(_.yarn))).
        needle(n, pattern(n).toPosition, noose._2)
    case (x, (n, _, ys)) =>
      //knit yarnB
      val (x2, noose) = x.withYarnB(_.to(n, true).noose)
      x2.knit(Stitch2(noose._1, noose._3, ys)).
        knit(n, PlainStitch.orEmpty(ys.map(_.yarn))).
        needle(n, pattern(n).toPosition, noose._2)
  }
}

/** Knitting on the double bed of with the K-carriage. */
private class KDoubleBed(takeback: Boolean, needles: NeedleStateRow) {
  private implicit def toSet[A](a: A): Set[A] = Set(a)

  def plain(x: ResultBuilder, n: Needle): ResultBuilder = (x, (n, needles(n).position, needles(n).yarn)) match {
    case (x, (_, NeedleA, _)) =>
      //don't knit A needles
      x
    case (x, (n, NeedleE, ys)) if !takeback =>
      //don't knit E needles if no needle pull back from E
      //TODO do we need to "prevent" falling down of yarn in the yarn feeder
      x.doubleBedNeedle(n, NeedleE, ys).knit(n, NoStitch)
    case (x, (n, _, ys)) =>
      //knit normally
      val (x2, (l, t, r)) = x.withYarnA(_.to(n, false).noose)
      x2.knit(Stitch2(r, ys, l)).
        knit(n, PurlStitch.orEmpty(ys.map(_.yarn))).
        doubleBedNeedle(n, NeedleB, t)
  }

  def part(x: ResultBuilder, n: Needle): ResultBuilder = (x, (n, needles(n).position, needles(n).yarn)) match {
    case (x, (_, NeedleA, _)) =>
      //don't knit A needles
      x
    case (x, (n, NeedleE, ys)) if !takeback =>
      //don't knit E needles if no needle pull back from E
      //TODO do we need to "prevent" falling down of yarn in the yarn feeder
      x.doubleBedNeedle(n, NeedleE, ys).knit(n, NoStitch)
    case (x, (n, NeedleB, ys)) =>
      // don't knit B needles with part
      x.doubleBedNeedle(n, NeedleB, ys)
    case (x, (n, _, ys)) =>
      //knit normally
      val (x2, noose) = x.withYarnA(_.to(n, false).noose)
      x2.knit(Stitch2(ys, noose._3, noose._1)).
        knit(n, PurlStitch.orEmpty(ys.map(_.yarn))).
        doubleBedNeedle(n, NeedleB, noose._2)
  }
}