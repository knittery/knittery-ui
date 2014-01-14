package models.plan

import scala.util.Try
import scalaz._
import Scalaz._
import models._
import utils._

/** Step to perform during knitting. */
sealed trait Step {
  def apply(on: KnittingState): Validation[String, KnittingState]
}

/** Knits a row using a carriage. */
case class KnitRow(carriage: CarriageType, direction: Direction, needleActionRow: Option[NeedleActionRow] = None) extends Step {
  override def apply(state: KnittingState) = {
    for {
      kc <- knittingCarriage(state, needleActionRow)
      (needles, knitted) <- kc(direction)(state.needles)
    } yield {
      state.
        moveCarriage(carriage, direction).
        modifyNeedles(needles).
        knit(knitted)
    }
  }
  protected def knittingCarriage(state: KnittingState, pattern: Option[NeedleActionRow]) = {
    for {
      pos <- state.carriagePosition.get(carriage).toSuccess(s"Undefined position for $carriage")
      settings <- state.carriageSettings.get(carriage).toSuccess(s"Undefined settings for $carriage")
      c = KnittingCarriage(carriage, settings, state.yarnA, state.yarnB, pattern)
      nextDir <- state.nextDirection(carriage)
      _ <- {
        if (nextDir != direction) s"Cannot move carriage from $direction to $direction".fail[KnittingCarriage]
        else ().success
      }
    } yield c
  }
}

/** Manual movement of needles. */
case class MoveNeedles(to: NeedlePatternRow) extends Step {
  override def apply(state: KnittingState) = {
    val toAButYarn = Needle.all.filter(n => to(n).nonWorking && state.needles(n).yarn.nonEmpty)
    if (toAButYarn.nonEmpty) s"Needles ${toAButYarn.mkString(", ")} have yarn and cannot be moved to A".fail
    else state.moveNeedles(to).success
  }
}
object MoveNeedles {
  /** Changes working needles to the values in the pattern. Non working are not touched. */
  def apply(before: NeedlePatternRow, pattern: NeedleActionRow) = new MoveNeedles(n =>
    if (before(n).isWorking) pattern(n).toPosition
    else before(n))
}

case class ChangeCarriageSettings(settings: CarriageSettings) extends Step {
  def carriage: CarriageType = settings.carriage
  override def apply(state: KnittingState) = {
    state.modifyCarriageSettings(settings).success[String]
  }
}

case class ThreadYarn(yarnA: Option[Yarn], yarnB: Option[Yarn]) extends Step {
  override def apply(state: KnittingState) = {
    state.copy(yarnA = yarnA, yarnB = yarnB).success[String]
  }
}

/**
 *  Performs a closed cast on for the needles. The needles are then moved to D position.
 *  All other needles are not touched.
 */
case class ClosedCastOn(from: Needle, until: Needle, yarn: Yarn) extends Step {
  def needles = Needle.interval(from, until)
  override def apply(state: KnittingState) = {
    state.
      modifyNeedles { n =>
        val before = state.needles(n)
        if (needles.contains(n)) {
          NeedleState(NeedleD, yarn :: before.yarn)
        } else before
      }.
      knit { n =>
        if (needles.contains(n)) CastOnStitch(yarn)
        else NoStitch
      }.
      success[String]
  }
}

case class ClosedCastOff(withYarn: Yarn, filter: Needle => Boolean) extends Step {
  override def apply(state: KnittingState) = {
    for {
      _ <- {
        if (state.yarns.contains(withYarn)) ().success
        else s"Cannot cast off with not threaded yarn $withYarn".fail
      }
      state2 = state.
        knit { n =>
          if (filter(n)) state.needles(n) match {
            case NeedleState(_, Nil) => NoStitch
            case NeedleState(_, yarns) => PlainStitch(yarns)
          }
          else NoStitch
        }.
        knit { n =>
          if (filter(n)) state.needles(n) match {
            case NeedleState(_, Nil) => NoStitch
            case NeedleState(_, yarns) => CastOffStitch(withYarn)
          }
          else NoStitch
        }.
        modifyNeedles(n => if (filter(n)) NeedleState(NeedleA) else state.needles(n))
    } yield state2
  }
}

case class AddCarriage(carriage: CarriageType, from: Direction = Left) extends Step {
  override def apply(state: KnittingState) =
    state.moveCarriage(carriage, from).success
}