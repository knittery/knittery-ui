package models.plan

import scala.util.{ Try, Success }
import models._

trait KnittingPlan {
  def steps: Seq[KnittingStep]

  def validate: Try[Unit] = {
    val initial: Try[KnittingState] = Success(KnittingState.initial)
    steps.foldLeft(initial) { (state, step) => state.flatMap(step.apply) }.
      map(_ => ())
  }
}