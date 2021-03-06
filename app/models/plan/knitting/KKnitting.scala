package models.plan.knitting

import scala.util.Try
import models._
import models.KCarriage._
import models.plan._
import utils._

class KKnitting(carriageState: State, state: KnittingState, direction: Direction) {
  def apply(pattern: NeedleActionRow) = Try {
    val main = mainBed(pattern)
    val double = doubleBed.getOrElse(noop(DoubleBed) _)
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
    val bed = new KMainBed(settings.holdingCamLever == HoldingCamN, pattern, state.needles(MainBed))
    val part = settings.part(direction)
    val tuck = settings.tuck(direction)
    (settings.mc, settings.l, part, tuck, carriageState.yarnA, carriageState.yarnB) match {
      case (false, false, false, false, Some(yarn), None) =>
        bed.plain _
      case (false, false, true, false, Some(yarn), None) =>
        bed.part _
      case (false, false, false, true, Some(yarn), None) =>
        throw new NotImplementedError("tuck knitting not yet implemented")
      case (true, false, false, false, Some(yarnA), _) =>
        bed.mc _
      case (_, true, false, false, Some(yarn), Some(yarnB)) =>
        throw new NotImplementedError("L knitting not yet implemented")
      case (_, _, false, false, None, None) =>
        throw new NotImplementedError("main-bed knitting with no yarn not yet implemented")
      case _ =>
        throw new IllegalStateException(s"Illegal settings for K (main bed). Settings: ${carriageState.settings}, yarns: ${carriageState.yarns}")
    }
  }

  private def doubleBed = carriageState.assembly match {
    case sp: SinkerPlate => None
    case dbc: DoubleBedCarriage => Some {
      require(!settings.mc, "MC not supported with double bed assembly.")
      require(!settings.l, "L not supported with double bed assembly.")
      val bed = new KDoubleBed(dbc.needleTakeback(direction), state.needles(DoubleBed))
      val part = dbc.part(direction)
      val knob = dbc.knob(direction)
      (part, dbc.tuckingLever, dbc.slideLever, knob) match {
        case (false, TuckingLeverR, slideLever, KRChangeKnobPlain) if slideLever != SlideLeverIiIi =>
          bed.plain _
        case (true, TuckingLeverR, slideLever, KRChangeKnobPlain) if slideLever != SlideLeverIiIi =>
          bed.part _
        case (true, TuckingLeverR, SlideLeverIiIi, KRChangeKnobIiIi) =>
          val working = state.workingNeedles(DoubleBed)
          val first = if (direction == ToRight) working.headOption else working.reverse.headOption
          val evens = first.map(_.index % 2 == 0).getOrElse(true)
          bed.IiIi(evens) _
        case (_, TuckingLeverP, slideLever, KRChangeKnobPlain) if slideLever != SlideLeverIiIi =>
          throw new NotImplementedError("english rib knitting / tucking pattern not yet implemented")
        case _ =>
          throw new IllegalStateException(s"Illegal settings for K (double bed). Settings: $dbc")
      }
    }
  }

  private def noop(bed: Bed)(x: ResultBuilder, n: Needle) = x.knit(n, bed, NoStitch)
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
      x.needle(n, NeedleE, ys).knit(n, MainBed, NoStitch)
    case (x, (n, _, ys)) =>
      //knit normally
      val (x2, (l, t, r)) = x.withYarnA(_.to(n, MainBed).noose)
      x2.knit(Stitch3D(l, ys, r), MainBed, n).
        knit(n, MainBed, PlainStitch.orEmpty(ys.map(_.yarn))).
        needle(n, pattern(n).toPosition, t)
  }

  def part(x: ResultBuilder, n: Needle): ResultBuilder = (x, (n, needles(n).position, needles(n).yarn)) match {
    case (x, (_, NeedleA, _)) =>
      //don't knit A needles
      x
    case (x, (n, NeedleE, ys)) if !takeback =>
      //don't knit E needles if no needle pull back from E
      //TODO do we need to "prevent" falling down of yarn in the yarn feeder
      x.needle(n, NeedleE, ys).knit(n, MainBed, NoStitch)
    case (x, (n, NeedleB, ys)) =>
      // don't knit B needles with part
      x.needle(n, pattern(n).toPosition, ys).
        knit(n, MainBed, NoStitch)
    case (x, (n, _, ys)) =>
      //knit normally
      val (x2, (l, t, r)) = x.withYarnA(_.to(n, MainBed).noose)
      x2.knit(Stitch3D(l, ys, r), MainBed, n).
        knit(n, MainBed, PlainStitch(ys.map(_.yarn).toList)).
        needle(n, pattern(n).toPosition, t)
  }

  def mc(x: ResultBuilder, n: Needle): ResultBuilder = (x, (n, needles(n).position, needles(n).yarn)) match {
    case (x, (_, NeedleA, _)) =>
      //don't knit A needles
      x
    case (x, (n, NeedleE, ys)) if !takeback =>
      //don't knit E needles if no needle pull back from E
      //TODO do we need to "prevent" falling down of yarn in the yarn feeder
      x.needle(n, NeedleE, ys).knit(n, MainBed, NoStitch)
    case (x, (n, NeedleB, ys)) =>
      //knit yarnA
      val (x2, (l, t, r)) = x.withYarnA(_.to(n, MainBed).noose)
      x2.knit(Stitch3D(l, ys, r), MainBed, n).
        knit(n, MainBed, PlainStitch.orEmpty(ys.map(_.yarn))).
        needle(n, pattern(n).toPosition, t)
    case (x, (n, _, ys)) =>
      //knit yarnB
      require(x.yarnB.isDefined, "MC knitting with needle at D without yarn in B")
      val (x2, (l, t, r)) = x.withYarnB(_.to(n, MainBed).noose)
      x2.knit(Stitch3D(l, ys, r), MainBed, n).
        knit(n, MainBed, PlainStitch.orEmpty(ys.map(_.yarn))).
        needle(n, pattern(n).toPosition, t)
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
      x.knit(n, DoubleBed, NoStitch).
        doubleBedNeedle(n, NeedleE, ys)
    case (x, (n, _, ys)) =>
      //knit normally
      val (x2, (l, t, r)) = x.withYarnA(_.to(n, DoubleBed).noose)
      x2.knit(Stitch3D(r, ys, l), DoubleBed, n).
        knit(n, DoubleBed, PurlStitch.orEmpty(ys.map(_.yarn))).
        doubleBedNeedle(n, NeedleB, t)
  }

  def part(x: ResultBuilder, n: Needle): ResultBuilder = (x, (n, needles(n).position, needles(n).yarn)) match {
    case (x, (_, NeedleA, _)) =>
      //don't knit A needles
      x
    case (x, (n, NeedleE, ys)) if !takeback =>
      //don't knit E needles if no needle pull back from E
      //TODO do we need to "prevent" falling down of yarn in the yarn feeder
      x.doubleBedNeedle(n, NeedleE, ys).knit(n, DoubleBed, NoStitch)
    case (x, (n, NeedleB, ys)) =>
      // don't knit B needles with part
      x.knit(n, DoubleBed, NoStitch).
        doubleBedNeedle(n, NeedleB, ys)
    case (x, (n, _, ys)) =>
      //knit normally
      val (x2, (l, t, r)) = x.withYarnA(_.to(n, DoubleBed).noose)
      x2.knit(Stitch3D(r, ys, l), DoubleBed, n).
        knit(n, DoubleBed, PurlStitch.orEmpty(ys.map(_.yarn))).
        doubleBedNeedle(n, NeedleB, t)
  }

  def IiIi(selectEven: Boolean)(x: ResultBuilder, n: Needle): ResultBuilder = {
    def position = if (n.index % 2 == 0 ^ selectEven) NeedleD else NeedleB
    (x, (n, needles(n).position, needles(n).yarn)) match {
      case (x, (_, NeedleA, _)) =>
        //don't knit A needles
        x
      case (x, (n, NeedleE, ys)) if !takeback =>
        //don't knit E needles if no needle pull back from E
        //TODO do we need to "prevent" falling down of yarn in the yarn feeder
        x.doubleBedNeedle(n, NeedleE, ys).knit(n, DoubleBed, NoStitch)
      case (x, (n, NeedleB, ys)) =>
        // don't knit B needles with part
        x.knit(n, DoubleBed, NoStitch).
          doubleBedNeedle(n, position, ys)
      case (x, (n, _, ys)) =>
        //knit normally
        val (x2, (l, t, r)) = x.withYarnA(_.to(n, DoubleBed).noose)
        x2.knit(Stitch3D(r, ys, l), DoubleBed, n).
          knit(n, DoubleBed, PurlStitch.orEmpty(ys.map(_.yarn))).
          doubleBedNeedle(n, position, t)
    }
  }
}