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
  yarn: Map[YarnPiece, YarnAttachment],
  knitted2: Knitted2 => Knitted2,
  stitches: Needle => Stitch)

private object KnittingCarriage {
  def apply(carriageState: CarriageState, yarnAttachments: Map[YarnPiece, YarnAttachment],
    pattern: NeedleActionRow): KnittingCarriage = carriageState match {
    case state: KCarriage.State =>
      new KKnittingCarriage(state.settings, state.yarnA, state.yarnB, yarnAttachments, pattern)
    case state: LCarriage.State =>
      new LKnittingCarriage(state.settings, pattern)
    case state: GCarriage.State =>
      new GKnittingCarriage(state.settings, state.yarn, pattern)
  }

  private class KKnittingCarriage(settings: KCarriage.Settings,
    yarnA: Option[YarnPiece], yarnB: Option[YarnPiece],
    yarnAttachments: Map[YarnPiece, YarnAttachment],
    pattern: NeedleActionRow)
    extends KnittingCarriage {
    import KCarriage._

    case class YarnFeeder(pos: YarnFlow, attachedTo: Option[(Needle, Int)]) {
      /** Straight yarn towards the needle. */
      def to(needle: Needle) =
        YarnFeeder(pos.next(distanceTo(needle)), Some(needle, 0))
      /** Make a noose at the current position. */
      def noose: (YarnFeeder, (YarnFlow, YarnFlow, YarnFlow)) = {
        val stream = pos.nexts(1)
        (copy(pos = stream(2)), (pos, stream(1), stream(2)))
      }
      def attachment = attachedTo.map(v => (pos.start, YarnAttachment(pos, v._1, v._2)))
      private def distanceTo(needle: Needle): Int = {
        attachedTo.map {
          case (n, rowDistance) => (n.index - needle.index).abs * 2 + rowDistance
        }.getOrElse(0)
      }
    }
    object YarnFeeder {
      def apply(ya: YarnAttachment): YarnFeeder =
        YarnFeeder(ya.yarn, Some(ya.needle, ya.rowDistance))
      def apply(yarn: YarnPiece): YarnFeeder =
        YarnFeeder(yarn, None)
    }

    case class ResultBuilder(
      yarnA: Option[YarnFeeder] = None,
      yarnB: Option[YarnFeeder] = None,
      needles: Map[Needle, NeedleState] = Map.empty,
      outputs: Seq[Stitch2] = Seq.empty,
      stitches: Map[Needle, Stitch] = Map.empty) {

      def withYarnA[X](f: YarnFeeder => (YarnFeeder, X)): (ResultBuilder, X) = {
        require(yarnA.isDefined, "No yarn A")
        val (y, r) = f(yarnA.get)
        (copy(yarnA = Some(y)), r)
      }
      def withYarnB[X](f: YarnFeeder => (YarnFeeder, X)): (ResultBuilder, X) = {
        require(yarnB.isDefined, "No yarn B")
        val (y, r) = f(yarnB.get)
        (copy(yarnB = Some(y)), r)
      }

      def needle(needle: Needle, state: NeedleState) =
        copy(needles = needles + (needle -> state))
      def needle(n: Needle, pos: NeedlePosition, ys: Set[YarnFlow]): ResultBuilder =
        needle(n, NeedleState(pos, ys))

      def knit(s: Stitch2): ResultBuilder = copy(outputs = outputs :+ s)
      def knit(n: Needle, s: Stitch) = copy(stitches = stitches + (n -> s))

      val yarnMap = (yarnA.flatMap(_.attachment).toList ++
        yarnB.flatMap(_.attachment).toList).toMap
      def toResult = KnittingCarriageResult(
        needles.withDefaultValue(NeedleState(NeedleA)),
        yarnMap,
        o => outputs.foldLeft(o)(_ + _),
        stitches.withDefaultValue(EmptyStitch))
    }
    object ResultBuilder {
      def apply(yarn: YarnPiece): ResultBuilder =
        ResultBuilder(yarnA = Some(yarnFeeder(yarn)))
      def apply(a: YarnPiece, b: YarnPiece): ResultBuilder =
        ResultBuilder(yarnA = Some(yarnFeeder(a)), yarnB = Some(yarnFeeder(b)))

      private def yarnFeeder(yarn: YarnPiece) = {
        yarnAttachments.get(yarn).map(YarnFeeder.apply).
          getOrElse(YarnFeeder(yarn))
      }
    }

    type LoopFun = (ResultBuilder, (Needle, NeedlePosition, Set[YarnFlow])) => ResultBuilder
    def loopNeedles(direction: Direction, needles: NeedleStateRow, initial: ResultBuilder)(f: LoopFun) = {
      val ns = if (direction == ToRight) Needle.all else Needle.all.reverse
      val r = ns.map(n => (n, needles(n).position, needles(n).yarn)).foldLeft(initial)(f)
      r.toResult
    }
    private implicit def toSet[A](a: A): Set[A] = Set(a)

    def knitPlain(direction: Direction, needles: NeedleStateRow, yarn: YarnPiece) = {
      loopNeedles(direction, needles, ResultBuilder(yarn)) {
        case (x, (_, NeedleA, _)) =>
          //don't knit A needles
          x
        case (x, (n, NeedleE, ys)) if settings.holdingCamLever != HoldingCamN =>
          //don't knit E needles if no needle pull back from E
          //TODO do we need to "prevent" falling down of yarn in the yarn feeder
          x.needle(n, NeedleE, ys)
        case (x, (n, _, ys)) =>
          //knit normally
          val (x2, noose) = x.withYarnA(_.to(n).noose)
          x2.knit(Stitch2(noose._1, noose._3, ys))
            .knit(n, PlainStitch(ys.map(_.yarn).toList)).
            needle(n, pattern(n).toPosition, noose._2)
      }
    }

    def knitMC(direction: Direction, needles: NeedleStateRow, a: YarnPiece, b: YarnPiece) = {
      loopNeedles(direction, needles, ResultBuilder(a, b)) {
        case (x, (_, NeedleA, _)) =>
          //don't knit A needles
          x
        case (x, (n, NeedleE, ys)) if settings.holdingCamLever != HoldingCamN =>
          //don't knit E needles if no needle pull back from E
          //TODO do we need to "prevent" falling down of yarn in the yarn feeder
          x.needle(n, NeedleE, ys)
        case (x, (n, NeedleB, ys)) =>
          //knit yarnA
          val (x2, noose) = x.withYarnA(_.to(n).noose)
          x2.knit(Stitch2(noose._1, noose._3, ys))
            .knit(n, PlainStitch(ys.map(_.yarn).toList)).
            needle(n, pattern(n).toPosition, noose._2)
        case (x, (n, _, ys)) =>
          //knit yarnB
          val (x2, noose) = x.withYarnB(_.to(n).noose)
          x2.knit(Stitch2(noose._1, noose._3, ys))
            .knit(n, PlainStitch(ys.map(_.yarn).toList)).
            needle(n, pattern(n).toPosition, noose._2)
      }
    }

    def knitPart(direction: Direction, needles: NeedleStateRow, yarn: YarnPiece) = {
      loopNeedles(direction, needles, ResultBuilder(yarn)) {
        case (x, (_, NeedleA, _)) =>
          //don't knit A needles
          x
        case (x, (n, NeedleE, ys)) if settings.holdingCamLever != HoldingCamN =>
          //don't knit E needles if no needle pull back from E
          //TODO do we need to "prevent" falling down of yarn in the yarn feeder
          x.needle(n, NeedleE, ys)
        case (x, (n, NeedleB, ys)) =>
          // don't knit B needles with part
          x.needle(n, pattern(n).toPosition, ys)
        case (x, (n, _, ys)) =>
          //knit normally
          val (x2, noose) = x.withYarnA(_.to(n).noose)
          x2.knit(Stitch2(noose._1, noose._3, ys))
            .knit(n, PlainStitch(ys.map(_.yarn).toList)).
            needle(n, pattern(n).toPosition, noose._2)
      }
    }

    override def apply(direction: Direction, needles: NeedleStateRow) = Try {
      (settings.part(direction), settings.tuck(direction), settings.mc, settings.l, yarnA, yarnB) match {
        case (false, false, false, false, Some(yarn), None) =>
          knitPlain(direction, needles, yarn)
        case (false, false, false, false, _, Some(_)) =>
          throw new IllegalArgumentException(s"Settings are illegal: Plain with yarn B threaded")
        case (false, false, false, false, None, _) => ??? //TODO remove the knitting from the board
        case (false, false, true, false, Some(a), Some(b)) =>
          knitMC(direction, needles, a, b)
        case (true, false, false, false, Some(yarn), _) =>
          knitPart(direction, needles, yarn)
        case (true, false, false, false, None, _) =>
          ??? // TODO
        case (false, true, false, false, _, _) => ??? //TODO tuck
        case (false, false, _, true, _, _) => ??? //TODO l-mode
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