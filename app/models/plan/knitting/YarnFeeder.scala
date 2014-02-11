package models.plan.knitting

import models._
import models.plan.YarnAttachment

private case class YarnFeeder(pos: YarnFlow, attachedTo: Option[(Needle, Int)]) {
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

private object YarnFeeder {
  def apply(ya: YarnAttachment): YarnFeeder =
    YarnFeeder(ya.yarn, Some(ya.needle, ya.rowDistance))
  def apply(yarn: YarnPiece): YarnFeeder =
    YarnFeeder(yarn, None)
}
