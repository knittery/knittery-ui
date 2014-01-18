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
case class KnitRow(carriage: Carriage, direction: Direction, needleActionRow: NeedleActionRow = AllNeedlesToB) extends Step {
  override def apply(state: KnittingState) = {
    for {
      kc <- knittingCarriage(state, Some(needleActionRow))
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
  override def hashCode = carriage.hashCode ^ direction.hashCode ^ needleActionRow.all.hashCode
  override def equals(o: Any) = o match {
    case o: KnitRow => o.carriage == carriage && o.direction == direction &&
      o.needleActionRow.all == needleActionRow.all
    case _ => false
  }
  override def toString = {
    val row = needleActionRow.all.map(_.toPosition.toString).mkString
    s"KnitRow($carriage,$direction,${row})"
  }
}

/** Manual movement of needles. */
case class MoveNeedles(to: NeedlePatternRow) extends Step {
  override def apply(state: KnittingState) = {
    val toAButYarn = Needle.all.filter(n => to(n).nonWorking && state.needles(n).yarn.nonEmpty)
    if (toAButYarn.nonEmpty) s"Needles ${toAButYarn.mkString(", ")} have yarn and cannot be moved to A".fail
    else state.moveNeedles(to).success
  }
  override def hashCode = to.all.hashCode
  override def equals(o: Any) = o match {
    case MoveNeedles(to2) => to.all == to2.all
    case _ => false
  }
}
object MoveNeedles {
  /** Changes working needles to the values in the pattern. Non working are not touched. */
  def apply(before: NeedlePatternRow, pattern: NeedleActionRow) = new MoveNeedles(n =>
    if (before(n).isWorking) pattern(n).toPosition
    else before(n))
}

sealed trait ChangeCarriageSettings extends Step
case class ChangeKCarriageSettings(settings: KCarriage.Settings) extends ChangeCarriageSettings {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(KCarriage)
    require(cs.position != CarriageRemoved, "Cannot set settings on non-active K-carriage")
    state.modifyCarriage(cs.copy(settings = settings))
  }.toSuccess
}
case class ChangeLCarriageSettings(settings: LCarriage.Settings) extends ChangeCarriageSettings {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(LCarriage)
    require(cs.position != CarriageRemoved, "Cannot set settings on non-active L-carriage")
    state.modifyCarriage(cs.copy(settings = settings))
  }.toSuccess
}
case class ChangeGCarriageSettings(settings: GCarriage.Settings) extends ChangeCarriageSettings {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(GCarriage)
    require(cs.position != CarriageRemoved, "Cannot set settings on non-active G-carriage")
    state.modifyCarriage(cs.copy(settings = settings))
  }.toSuccess
}

sealed trait ThreadYarn extends Step
case class ThreadYarnK(yarnA: Option[Yarn], yarnB: Option[Yarn]) extends ThreadYarn {
  import KCarriage._
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(KCarriage)
    require(cs.position != CarriageRemoved, "Cannot thread yarn on non-active K-carriage")
    val newAssembly = cs.assembly match {
      case a: SinkerPlate => a.copy(yarnA = yarnA, yarnB = yarnB)
    }
    state.modifyCarriage(cs.copy(assembly = newAssembly))
  }.toSuccess
}
case class ThreadYarnG(yarn: Option[Yarn]) extends ThreadYarn {
  import KCarriage._
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(GCarriage)
    require(cs.position != CarriageRemoved, "Cannot thread yarn on non-active G-carriage")
    state.modifyCarriage(cs.copy(yarn = yarn))
  }.toSuccess
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
    state.
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
      modifyNeedles(n => if (filter(n)) NeedleState(NeedleA) else state.needles(n)).
      success
  }
  override def hashCode = withYarn.hashCode ^ filter.all.hashCode
  override def equals(o: Any) = o match {
    case ClosedCastOff(y, f) => withYarn == y && filter.all == f.all
    case _ => false
  }
}

case class AddCarriage(carriage: Carriage, at: LeftRight = Left) extends Step {
  override def apply(state: KnittingState) = Try {
    require(state.carriageState(carriage).position == CarriageRemoved,
      s"Can only add removed $carriage-carriage")
    val pos = if (at == Left) CarriageLeft(0) else CarriageRight(0)
    state.modifyCarriage(carriage match {
      case KCarriage => state.carriageState(KCarriage).copy(position = pos)
      case LCarriage => state.carriageState(LCarriage).copy(position = pos)
      case GCarriage => state.carriageState(GCarriage).copy(position = pos)
    })
  }.toSuccess
}