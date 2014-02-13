package models.plan.knitting

import models._
import models.plan.YarnAttachment

private case class YarnFeeder(pos: YarnFlow, attachedTo: Option[(Needle, Int, Bed)]) {
  /** Straight yarn towards the needle. */
  def to(needle: Needle, bed: Bed) =
    YarnFeeder(pos.next(distanceTo(needle, bed)), Some(needle, 0, bed))
  /** Make a noose at the current position. */
  def noose: (YarnFeeder, (YarnFlow, YarnFlow, YarnFlow)) = {
    val stream = pos.nexts(1)
    (copy(pos = stream(2)), (pos, stream(1), stream(2)))
  }
  def attachment = attachedTo.map(v => (pos.start, YarnAttachment(pos, v._1, v._3, v._2)))
  private def distanceTo(needle: Needle, bed: Bed): Int = {
    attachedTo.map {
      case (n, rowDistance, b) =>
        val bedChange = if (bed != b) 1 else 0
        val horizontal = (n.index - needle.index).abs * 2
        horizontal + rowDistance + bedChange
    }.getOrElse(0)
  }
}

private object YarnFeeder {
  def apply(ya: YarnAttachment): YarnFeeder =
    YarnFeeder(ya.yarn, Some(ya.needle, ya.rowDistance, ya.bed))
  def apply(yarn: YarnPiece): YarnFeeder =
    YarnFeeder(yarn, None)
}
