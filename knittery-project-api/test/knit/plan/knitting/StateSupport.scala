package knit.plan.knitting

import knit.plan.{YarnAttachment, KnittingState, NeedleState}
import knit.{MainBed, YarnFlow, NeedlePosition, Needle}

private trait StateSupport {
  def stateWithYarn(pos: Needle => NeedlePosition, yarnToUse: YarnFlow) = {
    val (yarn, ns) = Needle.all.foldLeft((yarnToUse, Map.empty[Needle, NeedleState])) {
      case ((yarn, r), n) =>
        val p = pos(n)
        if (p.isWorking) {
          val yarn2 = yarn.next(1)
          (yarn2, r + (n -> NeedleState(p, yarn2)))
        } else {
          (yarn, r + (n -> NeedleState(p)))
        }
    }
    KnittingState.initial.modifyNeedles(MainBed, ns).
      attachYarn(YarnAttachment(yarn, Needle.all.reverse.head, MainBed))
  }
}