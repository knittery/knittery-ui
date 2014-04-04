package models.guide

import models.{Bed, Needle}
import models.plan._


case class GuideStep private(title: Text, description: Text, wiki: WikiReference,
                             instructions: Seq[Instruction],
                             isKnitting: Boolean,
                             before: KnittingState, after: KnittingState,
                             position: Pos)
object GuideStep {
  def apply(title: Text, description: Text, wiki: WikiReference, instructions: Seq[Instruction], isKnitting: Boolean,
            before: KnittingState, after: KnittingState): GuideStep = {
    GuideStep(title, description, wiki, Instruction.updatePos(instructions), isKnitting, before, after, Pos.only)
  }

  private[guide] def updatePos(steps: Seq[GuideStep]) = {
    steps.zipWithIndex.map {
      case (step, index) => step.copy(position = Pos(index, steps.size))
    }
  }
}

case class Instruction(text: Text, step: Step,
                       markNeedles: Set[(Bed, Needle)],
                       before: KnittingState, after: KnittingState,
                       position: Pos = Pos.only)
object Instruction {
  private[guide] def updatePos(instructions: Seq[Instruction]) = {
    instructions.zipWithIndex.map {
      case (instr, index) => instr.copy(position = Pos(index, instructions.size))
    }
  }
}

case class Pos(index: Int, count: Int) extends Ordered[Pos] {
  require(index < count && index >= 0, s"$index is not valid (count=$count)")
  def isFirst = index == 0
  def isLast = index >= count - 1
  def shift(by: Int) = copy(index = index + by)
  def shiftOption(by: Int) = {
    Some(index + by).
      filter(_ >= 0).filter(_ < count).
      map(copy(_))
  }
  override def compare(that: Pos) = index - that.index
}
object Pos {
  val only = Pos(0, 1)
}