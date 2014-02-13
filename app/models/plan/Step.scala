package models.plan

import scala.util.Try
import scalaz._
import Scalaz._
import models._
import utils._
import models.plan.knitting._

/** Step to perform during knitting. */
sealed trait Step {
  def apply(on: KnittingState): Validation[String, KnittingState]
}

/** Knits a row using a carriage. */
case class KnitRow(carriage: Carriage, direction: Direction, pattern: NeedleActionRow = AllNeedlesToB) extends Step {
  override def apply(state: KnittingState) = for {
    nextDir <- state.nextDirection(carriage)
    _ <- {
      if (nextDir != direction) s"Cannot move carriage from $direction to $direction".fail
      else ().success
    }
    state2 <- carriage match {
      case KCarriage =>
        val knitting = new KKnitting(state.carriageState(KCarriage), state, direction)
        knitting(pattern)
      case LCarriage => ???
      case GCarriage => ???
    }
  } yield state2.moveCarriage(carriage, direction)

  override def hashCode = carriage.hashCode ^ direction.hashCode ^ pattern.all.hashCode
  override def equals(o: Any) = o match {
    case o: KnitRow => o.carriage == carriage && o.direction == direction &&
      o.pattern.all == pattern.all
    case _ => false
  }
  override def toString = {
    val row = pattern.all.map(_.toPosition.toString).mkString
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

/** Manual movement of needles on the double bed. */
case class MoveNeedlesDoubleBed(to: NeedlePatternRow) extends Step {
  override def apply(state: KnittingState) = {
    val toAButYarn = Needle.all.filter(n => to(n).nonWorking && state.doubleBedNeedles(n).yarn.nonEmpty)
    if (toAButYarn.nonEmpty) s"Needles ${toAButYarn.mkString(", ")} on double bed have yarn and cannot be moved to A".fail
    else state.moveDoubleBedNeedles(to).success
  }
  override def hashCode = to.all.hashCode
  override def equals(o: Any) = o match {
    case MoveNeedles(to2) => to.all == to2.all
    case _ => false
  }
}

sealed trait ChangeCarriageSettings extends Step {
  def carriage: Carriage
}
case class ChangeKCarriageSettings(settings: KCarriage.Settings) extends ChangeCarriageSettings {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(KCarriage)
    require(cs.position != CarriageRemoved, "Cannot set settings on non-active K-carriage")
    state.modifyCarriage(cs.copy(settings = settings))
  }.toSuccess
  override def carriage = KCarriage
}
case class ChangeKCarriageAssembly(assembly: KCarriage.Assembly) extends ChangeCarriageSettings {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(KCarriage)
    require(cs.position != CarriageRemoved, "Cannot set assembly on non-active K-carriage")
    state.modifyCarriage(cs.copy(assembly = assembly))
  }.toSuccess
  override def carriage = KCarriage
}
case class ChangeLCarriageSettings(settings: LCarriage.Settings) extends ChangeCarriageSettings {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(LCarriage)
    require(cs.position != CarriageRemoved, "Cannot set settings on non-active L-carriage")
    state.modifyCarriage(cs.copy(settings = settings))
  }.toSuccess
  override def carriage = LCarriage
}
case class ChangeGCarriageSettings(settings: GCarriage.Settings) extends ChangeCarriageSettings {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(GCarriage)
    require(cs.position != CarriageRemoved, "Cannot set settings on non-active G-carriage")
    state.modifyCarriage(cs.copy(settings = settings))
  }.toSuccess
  override def carriage = GCarriage
}

sealed trait ThreadYarn extends Step
case class ThreadYarnK(yarnA: Option[YarnPiece], yarnB: Option[YarnPiece]) extends ThreadYarn {
  import KCarriage._
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(KCarriage)
    require(cs.position != CarriageRemoved, "Cannot thread yarn on non-active K-carriage")
    require(yarnA.isEmpty || yarnA != yarnB, "Cannot thread same piece of yarn to A and B")
    val newAssembly = cs.assembly match {
      case a: SinkerPlate => a.copy(yarnA = yarnA, yarnB = yarnB)
      case a: DoubleBedCarriage =>
        require(yarnB.isEmpty, "Cannot thread two yarns in the double bed carriage")
        a.copy(yarn = yarnA)
    }
    state.modifyCarriage(cs.copy(assembly = newAssembly))
  }.toSuccess
}
case class ThreadYarnG(yarn: Option[YarnPiece]) extends ThreadYarn {
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
case class ClosedCastOn(from: Needle, until: Needle, yarn: YarnPiece) extends Step {
  def needles = Needle.interval(from, until)
  def direction: Direction = if (from < until) ToLeft else ToRight
  val width = 1
  override def apply(state: KnittingState) = {
    val yarnFlow = yarn.nexts(width)
    val needleYarn = needles.zip(yarnFlow).toMap
    state.
      modifyNeedles { n =>
        val before = state.needles(n)
        needleYarn.get(n).map { yarn =>
          NeedleState(NeedleD, before.yarn + yarn)
        }.getOrElse(before)
      }.
      knit { n =>
        if (needles.contains(n)) CastOnStitch(yarn.yarn)
        else NoStitch
      }.
      //TODO Knit2? we don't really have stitches..
      attachYarn(YarnAttachment(needleYarn(until), until)).
      success[String]
  }
}

case class ClosedCastOff(withYarn: YarnPiece, filter: Needle => Boolean) extends Step {
  override def apply(state: KnittingState) = {
    state.
      knit { n =>
        if (filter(n)) state.needles(n) match {
          case NeedleState(_, yarns) if yarns.isEmpty => NoStitch
          case NeedleState(_, yarns) => PlainStitch(yarns.map(_.yarn).toList)
        }
        else NoStitch
      }.
      knit { n =>
        if (filter(n)) state.needles(n) match {
          case NeedleState(_, yarns) if yarns.isEmpty => NoStitch
          case NeedleState(_, yarns) => CastOffStitch(withYarn.yarn)
        }
        else NoStitch
      }.
      //TODO implement .knit2
      modifyNeedles(n => if (filter(n)) NeedleState(NeedleA) else state.needles(n)).
      pushRow(filter).
      detachYarn(withYarn).
      success
  }
  override def hashCode = withYarn.hashCode ^ filter.all.hashCode
  override def equals(o: Any) = o match {
    case ClosedCastOff(y, f) => withYarn == y && filter.all == f.all
    case _ => false
  }
}

/** Moves the yarn from the main to the double bed. Needles affected are moved to B position. */
case class MoveToDoubleBed(filter: Needle => Boolean, offset: Int = 0, flip: Option[Needle] = None) extends Step {
  override def apply(state: KnittingState) = {
    val (nm, nd) = Needle.all.foldLeft((state.needles, state.doubleBedNeedles)) {
      case ((main, double), fromN) => movementTarget(fromN).map { toN =>
        if (main(fromN).position.isWorking) {
          val m = NeedleState(NeedleB)
          val d = NeedleState(NeedleB, double(toN).yarn ++ main(fromN).yarn)
          (main + (fromN -> m), double + (toN -> d))
        } else (main, double)
      }.getOrElse(main, double)
    }
    state.copy(needles = nm, doubleBedNeedles = nd).success
  }
  def movementTarget(from: Needle) = {
    val index = flip match {
      case Some(flipAt) => flipAt.index + (flipAt.index - from.index)
      case None => from.index + offset
    }
    Some(index).filter(i => i >= 0 && i < Needle.count).map(Needle.atIndex)
  }
}

case class AddCarriage(carriage: Carriage, at: LeftRight = Left) extends Step {
  override def apply(state: KnittingState) = Try {
    require(state.carriageState(carriage).position == CarriageRemoved,
      s"Can only add removed $carriage-carriage")
    state.moveCarriage(carriage, at)
  }.toSuccess
}

case class Information(title: String, description: String) extends Step {
  override def apply(state: KnittingState) = state.success
}