package models.plan.knitting

import models._
import models.plan._

private case class ResultBuilder(
  yarnA: Option[YarnFeeder] = None,
  yarnB: Option[YarnFeeder] = None,
  needles: Map[Needle, NeedleState] = Map.empty,
  doubleBedNeedles: Map[Needle, NeedleState] = Map.empty,
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
  def doubleBedNeedle(needle: Needle, state: NeedleState) =
    copy(doubleBedNeedles = doubleBedNeedles + (needle -> state))
  def doubleBedNeedle(n: Needle, pos: NeedlePosition, ys: Set[YarnFlow]): ResultBuilder =
    doubleBedNeedle(n, NeedleState(pos, ys))

  def knit(s: Stitch2): ResultBuilder = copy(outputs = outputs :+ s)
  def knit(n: Needle, s: Stitch) = copy(stitches = stitches + (n -> s))

  val yarnMap = (yarnA.flatMap(_.attachment).toList ++
    yarnB.flatMap(_.attachment).toList).toMap

  def execute(state: KnittingState) = {
    yarnMap.values.foldLeft(state)(_.attachYarn(_)).
      modifyNeedles(MainBed, needles.withDefaultValue(NeedleState(NeedleA))).
      modifyNeedles(DoubleBed, doubleBedNeedles.withDefaultValue(NeedleState(NeedleA))).
      knit(stitches.withDefaultValue(EmptyStitch)).
      knit2(_ ++ outputs)
  }
}

private object ResultBuilder {
  def apply(a: Option[YarnPiece], b: Option[YarnPiece], yas: Map[YarnPiece, YarnAttachment]): ResultBuilder =
    ResultBuilder(yarnA = a.map(yarnFeeder(yas)), yarnB = b.map(yarnFeeder(yas)))
  private def yarnFeeder(yarnAttachments: Map[YarnPiece, YarnAttachment])(yarn: YarnPiece) = {
    yarnAttachments.get(yarn).map(YarnFeeder.apply).
      getOrElse(YarnFeeder(yarn))
  }
}
