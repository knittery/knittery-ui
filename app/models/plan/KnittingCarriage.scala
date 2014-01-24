package models.plan

import scala.util.Try
import scalaz._
import Scalaz._
import models._
import utils._

private trait KnittingCarriage {
  def apply(direction: Direction, needles: NeedleStateRow): Validation[String, KnittingCarriageResult]
}

private case class KnittingCarriageResult(
  needles: NeedleStateRow,
  knitted2: Knitted2 => Knitted2,
  stitches: Needle => Stitch)

private object KnittingCarriage {
  def apply(carriageState: CarriageState, yarnAttachments: Map[YarnStart, YarnAttachment],
    pattern: NeedleActionRow): KnittingCarriage = carriageState match {
    case state: KCarriage.State =>
      new KKnittingCarriage(state.settings, state.yarnA, state.yarnB, yarnAttachments, pattern)
    case state: LCarriage.State =>
      new LKnittingCarriage(state.settings, pattern)
    case state: GCarriage.State =>
      new GKnittingCarriage(state.settings, state.yarn, pattern)
  }

  private def workInterval(direction: Direction, needles: NeedleStateRow) = {
    val ns = if (direction == ToRight) Needle.all else Needle.all.reverse
    val working = ns.filter(needles(_).position.isWorking)
    if (working.isEmpty) Seq.empty
    else Needle.interval(working.head, working.last)
  }

  private class KKnittingCarriage(settings: KCarriage.Settings,
    yarnA: Option[YarnFlow], yarnB: Option[YarnFlow],
    yarnAttachments: Map[YarnStart, YarnAttachment],
    pattern: NeedleActionRow)
    extends KnittingCarriage {
    import KCarriage._

    private def distanceTo(needle: Needle, yarn: YarnFlow): Int = {
      yarnAttachments.get(yarn.start).map { ya =>
        ya.rowDistance / 2 +
          (needle.index - ya.needle.index).abs
      }.getOrElse(0)
    }
    private def knittingDistance: Int = 1

    private case class ResultBuilder(yarnEnd: YarnFlow,
      needles: Map[Needle, NeedleState] = Map.empty,
      outputs: Seq[Knitted2 => Knitted2] = Seq.empty,
      stitches: Map[Needle, Stitch] = Map.empty) {
      def knitFlat = knit(yarnStream(0)).copy(yarnEnd = yarnStream(2))
      def noose = {
        val next = copy(yarnEnd = yarnStream(2)).
          knit(yarnEnd).knit(yarnStream(0)).knit(yarnStream(1))
        (yarnEnd, yarnStream(0), yarnStream(1), next)
      }
      def yarnStream = yarnEnd.nexts(knittingDistance)

      def needle(needle: Needle, state: NeedleState) =
        copy(needles = needles + (needle -> state))
      def needle(n: Needle, pos: NeedlePosition, ys: List[YarnFlow]): ResultBuilder =
        needle(n, NeedleState(pos, ys))

      def knit(f: Knitted2 => Knitted2) = copy(outputs = outputs :+ f)
      def knit(flow: YarnFlow): ResultBuilder = knit(_ + flow)
      def knit(s: Stitch2): ResultBuilder = knit(_ + s)
      def knit(n: Needle, s: Stitch) = copy(stitches = stitches + (n -> s))

      def toResult = KnittingCarriageResult(
        needles.withDefaultValue(NeedleState(NeedleA, Nil)),
        outputs.fold((x: Knitted2) => x)(_.andThen(_)),
        stitches.withDefaultValue(EmptyStitch))
    }

    private def knitPlain(direction: Direction, needles: NeedleStateRow, yarn: YarnFlow) = {
      val work = workInterval(direction, needles)
      if (work.nonEmpty) {
        val yarn0 = yarn.next(distanceTo(work.head, yarn))
        val result = work.map {
          case n => (n, needles(n).position, needles(n).yarn)
        }.foldLeft(ResultBuilder(yarn0)) {
          case (x, (_, NeedleA, _)) =>
            //don't knit A needles (but will still use a yarn point, opposed to those out of the knitting area)
            x.knitFlat
          case (x, (n, NeedleE, ys)) if settings.holdingCamLever != HoldingCamN =>
            //don't knit E needles if no needle pull back from E
            x.needle(n, NeedleE, ys).
              knitFlat.knit(n, NoStitch)
          case (x, (n, _, ys)) =>
            //knit normally
            val (l, noose, r, x2) = x.noose
            x2.knit(Stitch2(Set(l), ys.toSet, Set(r))).
              knit(n, PlainStitch(ys.map(_.yarn))).
              needle(n, pattern(n).toPosition, List(noose))
        }
        result.toResult
      } else {
        // No knitting, because no active needles
        KnittingCarriageResult(needles, identity, _ => NoStitch)
      }
    }

    private def knitMC(direction: Direction, needles: NeedleStateRow) = {
      ???
    }

    override def apply(direction: Direction, needles: NeedleStateRow) = Try {
      (settings.part(direction), settings.tuck(direction), settings.mc, settings.l, yarnA) match {
        case (false, false, false, false, Some(yarn)) => knitPlain(direction, needles, yarn)
        case (false, false, false, false, None) => ??? //TODO remove the knitting from the board
        case (false, false, true, false, _) => knitMC(direction, needles)
        case (true, false, false, false, _) => ??? //TODO part
        case (false, true, false, false, _) => ??? //TODO tuck
        case (false, false, _, true, _) => ??? //TODO l-mode
        case _ => throw new IllegalArgumentException(s"Settings are illegal: $settings")
      }
    }
  }

  private class LKnittingCarriage(settings: LCarriage.Settings, pattern: NeedleActionRow)
    extends KnittingCarriage {

    def apply(direction: Direction, needles: NeedleStateRow) = Try {
      if (needles.pattern.all.exists(_ == NeedleE))
        throw new IllegalStateException("LCarriage does not work with needles at E")

      //TODO implement L knitting
      ???
    }
  }

  private class GKnittingCarriage(settings: GCarriage.Settings, yarnA: Option[YarnFlow], pattern: NeedleActionRow)
    extends KnittingCarriage {

    def apply(direction: Direction, needles: NeedleStateRow) = Try {
      if (needles.pattern.all.exists(_ == NeedleE))
        throw new IllegalStateException("GCarriage does not work with needles at E")
      if (needles.pattern.all.exists(_ == NeedleD))
        throw new IllegalStateException("GCarriage does not work with needles at D")

      //TODO implement G knitting
      ???
    }
  }
}