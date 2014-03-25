package models.guide

import models.plan._


case class GuideStep private(title: Text, description: Text,
                             instructions: Seq[Instruction],
                             before: KnittingState, after: KnittingState,
                             position: Pos)
object GuideStep {
  def apply(title: Text, description: Text, instructions: Seq[Instruction],
            before: KnittingState, after: KnittingState): GuideStep = {
    GuideStep(title, description, Instruction.updatePos(instructions), before, after, Pos.only)
  }

  private[guide] def updatePos(steps: Seq[GuideStep]) = {
    steps.zipWithIndex.map {
      case (step, index) => step.copy(position = Pos(index, steps.size))
    }
  }
}

case class Instruction(text: Text,
                       before: KnittingState, after: KnittingState,
                       position: Pos = Pos.only)
object Instruction {
  private[guide] def updatePos(instructions: Seq[Instruction]) = {
    instructions.zipWithIndex.map {
      case (instr, index) => instr.copy(position = Pos(index, instructions.size))
    }
  }
}

case class Pos(index: Int, count: Int) {
  def first = index == 0
  def last = index >= count - 1
}
object Pos {
  val only = Pos(0, 1)
}