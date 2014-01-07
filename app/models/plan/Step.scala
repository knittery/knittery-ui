package models.plan

import scala.util.Try
import scalaz._
import Scalaz._
import models._
import utils._

/** Step to perform during knitting. */
sealed trait Step extends (KnittingState => Validation[String, KnittingState])

/** Knits a row using a carriage. */
sealed trait KnitARow extends Step {
  def carriage: CarriageType
  def direction: Direction
  protected def needleActionRow: Option[NeedleActionRow]

  override def apply(state: KnittingState) = {
    for {
      kc <- knittingCarriage(state, needleActionRow)
      (needles, knitted) <- kc(direction)(state.needles)
    } yield {
      state.
        moveCarriage(carriage, direction).
        moveNeedles(needles).
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

case class KnitRow(carriage: CarriageType, direction: Direction) extends KnitARow {
  override protected def needleActionRow = None
}

case class KnitPatternRow(carriage: CarriageType, direction: Direction, pattern: NeedleActionRow) extends KnitARow {
  override protected def needleActionRow = Some(pattern)
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
      moveNeedles { n =>
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
        moveNeedles(n => if (filter(n)) NeedleState(NeedleA) else state.needles(n))
    } yield state2
  }
}

case class AddCarriage(carriage: CarriageType, from: Direction = Left) extends Step {
  override def apply(state: KnittingState) =
    state.moveCarriage(carriage, from.reverse).success
}