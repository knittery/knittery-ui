package models.guide

import models.plan._


case class GuideStep(title: Text, description: Text,
                     instructions: Seq[Instruction],
                     before: KnittingState, after: KnittingState)

case class Instruction(text: Text,
                       before: KnittingState, after: KnittingState)



