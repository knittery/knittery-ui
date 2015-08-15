package knit.plan.knitting

import knit.LCarriage

import scala.util.Try
import knit._
import LCarriage._
import knit.plan.{NeedleState, KnittingState}
import utils._

class LKnitting(carriageState: State, initial: KnittingState, direction: Direction) {
  def apply(pattern: NeedleActionRow) = Try {
    val needles = direction match {
      case ToRight => Needle.all
      case ToLeft => Needle.all.reverse
    }
    val (stateAfterKnitting, endCarry) = needles.foldLeft((initial, Set.empty[YarnFlow])) {
      case ((state, carry), needle) =>
        val needleRow = state.needles(MainBed)
        val (ns, c2) = handleNeedle(needle, needleRow(needle), carry, pattern(needle))
        val s2 = state.modifyNeedles(MainBed, needleRow.toMap + (needle -> ns))
        (s2, c2)
    }
    if (endCarry.nonEmpty) throw new IllegalStateException("Cannot move yarn from selvage outwards with L carriage")
    stateAfterKnitting
  }.toSuccess

  private def handleNeedle(needle: Needle, state: NeedleState, carry: Set[YarnFlow], target: NeedleAction): (NeedleState, Set[YarnFlow]) = state match {
    case NeedleState(NeedleE, _) =>
      throw new IllegalStateException(s"Needle $needle is in E position when knitting with L carriage")
    case NeedleState(NeedleA, _) =>
      (state, Set.empty)
    case NeedleState(NeedleB, yarns) =>
      (NeedleState(target.toPosition, yarns ++ carry), Set.empty)
    case NeedleState(NeedleD, _) if carry.nonEmpty =>
      throw new IllegalStateException(s"Two adjacent needles in D position when knitting with L carriage")
    case NeedleState(NeedleD, yarns) =>
      (NeedleState(target.toPosition), yarns)
  }
}
