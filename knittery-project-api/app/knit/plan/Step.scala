package knit.plan

import scala.util.Try
import scalaz._
import Scalaz._
import scalaz.Validation.FlatMap._
import knit._
import knitting.{KKnitting, LKnitting}
import utils._

/** Step to perform during knitting. */
sealed trait Step {
  def apply(on: KnittingState): Validation[String, KnittingState]

  def andThen(next: Step): Step = {
    val first = this
    new Step {
      def apply(on: KnittingState) = first(on).flatMap(next.apply)
    }
  }
}

/** Steps that must not be optimized. */
trait NonOptimizable
object NonOptimizable {
  def apply(step: Step) = step match {
    case _: NonOptimizable => true
    case _ => false
  }
}

/** Knits a row using a carriage. */
case class KnitRow(carriage: Carriage, direction: Direction, pattern: NeedleActionRow = AllNeedlesToB) extends Step {
  override def apply(state: KnittingState) = for {
    nextDir <- state.nextDirection(carriage)
    _ <- {
      if (nextDir != direction) s"Cannot move carriage from $direction to $direction".failure
      else ().success
    }
    state2 <- carriage match {
      case KCarriage =>
        val knitting = new KKnitting(state.carriageState(KCarriage), state, direction)
        knitting(pattern)
      case LCarriage =>
        val knitting = new LKnitting(state.carriageState(LCarriage), state, direction)
        knitting(pattern)
      case GCarriage =>
        throw new NotImplementedError("GCarriage knitting not yet implemented")
    }
  } yield state2.
      moveCarriage(carriage, direction).
      pushRow(_ => true)

  override def hashCode = carriage.hashCode ^ direction.hashCode ^ pattern.all.hashCode
  override def equals(o: Any) = o match {
    case o: KnitRow => o.carriage == carriage && o.direction == direction &&
      o.pattern.all == pattern.all
    case _ => false
  }
  override def toString = {
    val row = pattern.all.map(_.toPosition.toString).mkString
    s"KnitRow($carriage,$direction,$row)"
  }
}

/** Manual movement of needles. */
case class MoveNeedles(bed: Bed, to: NeedlePatternRow) extends Step {
  override def apply(state: KnittingState) = {
    val toAButYarn = Needle.all.filter(n => to(n).nonWorking && state.needles(bed)(n).yarn.nonEmpty)
    if (toAButYarn.nonEmpty) s"Needles ${toAButYarn.mkString(", ")} have yarn and cannot be moved to A".failure
    else state.moveNeedles(bed, to).success
  }
  override def hashCode = bed.hashCode ^ to.all.hashCode
  override def equals(o: Any) = o match {
    case MoveNeedles(bed2, to2) => bed == bed2 && to.all == to2.all
    case _ => false
  }
}
object MoveNeedles {
  /** Changes working needles to the values in the pattern. Non working are not touched. */
  def apply(before: NeedlePatternRow, pattern: NeedleActionRow, moveE: Boolean) = {
    def filter(pos: NeedlePosition) = pos.isWorking && (pos != NeedleE || moveE)
    new MoveNeedles(MainBed, n =>
      if (filter(before(n))) pattern(n).toPosition
      else before(n))
  }
}

sealed trait ChangeCarriageSettings extends Step {
  def carriage: Carriage
}
case class ChangeKCarriageSettings(settings: KCarriage.Settings, assembly: KCarriage.Assembly) extends ChangeCarriageSettings {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(KCarriage)
    require(cs.position != CarriageRemoved, "Cannot set settings on non-active K-carriage")
    state.modifyCarriage(cs.copy(settings = settings, assembly = assembly))
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
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(KCarriage)
    require(cs.position != CarriageRemoved, "Cannot thread yarn on non-active K-carriage")
    require(yarnA.isEmpty || yarnA != yarnB, "Cannot thread same piece of yarn to A and B")
    state.modifyCarriage(cs.copy(yarnA = yarnA, yarnB = yarnB))
  }.toSuccess
}
case class ThreadYarnG(yarn: Option[YarnPiece]) extends ThreadYarn {
  override def apply(state: KnittingState) = Try {
    val cs = state.carriageState(GCarriage)
    require(cs.position != CarriageRemoved, "Cannot thread yarn on non-active G-carriage")
    state.modifyCarriage(cs.copy(yarn = yarn))
  }.toSuccess
}

/**
 * Performs a closed cast on for the needles. The needles are then moved to D position.
 * All other needles are not touched.
 */
case class ClosedCastOn(bed: Bed, from: Needle, until: Needle, yarn: YarnPiece) extends Step {
  def needles = Needle.interval(from, until)
  def direction: Direction = if (from < until) ToLeft else ToRight
  val width = 1
  override def apply(state: KnittingState) = {
    val yarnFlow = yarn.nexts(width)
    val needleYarn = needles.zip(yarnFlow).toMap
    state.
      modifyNeedles(bed, { n =>
      val before = state.needles(bed)(n)
      needleYarn.get(n).map { yarn =>
        NeedleState(NeedleD, before.yarn + yarn)
      }.getOrElse(before)
    }).
      knit(
        n => {
          if (bed == MainBed && needles.contains(n)) CastOnStitch(yarn.yarn)
          else NoStitch
        },
        n => {
          if (bed == DoubleBed && needles.contains(n)) CastOnStitch(yarn.yarn)
          else NoStitch
        }
      ).
      //TODO Knit2? we don't really have stitches..
      attachYarn(YarnAttachment(needleYarn(until), until, MainBed)).
      pushRow(needles.contains).
      success[String]
  }
}

case class ClosedCastOff(bed: Bed, withYarn: YarnPiece, filter: Needle => Boolean) extends Step with NonOptimizable {
  override def apply(state: KnittingState) = {
    val needles = state.needles(bed)

    def knitLast(n: Needle) = {
      if (filter(n)) needles(n) match {
        case NeedleState(_, yarns) if yarns.isEmpty => EmptyStitch
        case NeedleState(_, yarns) => PlainStitch(yarns.map(_.yarn).toList)
      } else if (needles(n).yarn.isEmpty) EmptyStitch
      else NoStitch
    }
    def knitCastOff(n: Needle) = {
      if (filter(n)) needles(n) match {
        case NeedleState(_, yarns) if yarns.isEmpty => EmptyStitch
        case NeedleState(_, yarns) => CastOffStitch(withYarn.yarn)
      } else if (needles(n).yarn.isEmpty) EmptyStitch
      else NoStitch
    }

    state.
      knit(
        if (bed == MainBed) knitLast else _ => NoStitch,
        if (bed == DoubleBed) knitLast else _ => NoStitch
      ).
      knit(
        if (bed == MainBed) knitCastOff else _ => NoStitch,
        if (bed == DoubleBed) knitCastOff else _ => NoStitch
      ).
      //TODO implement .knit2
      modifyNeedles(bed, n => if (filter(n)) NeedleState(NeedleA) else needles(n)).
      pushRow(filter).
      detachYarn(withYarn).
      success
  }
  override def hashCode = bed.hashCode ^ withYarn.hashCode ^ filter.all.hashCode
  override def equals(o: Any) = o match {
    case ClosedCastOff(b, y, f) => bed == b && withYarn == y && filter.all == f.all
    case _ => false
  }
}

/** Moves the yarn from the main to the double bed. Needles affected are moved to B position. */
case class MoveToDoubleBed(filter: Needle => Boolean, offset: Int = 0, flip: Option[Needle] = None) extends Step {
  override def apply(state: KnittingState) = {
    val (nm, nd) = Needle.all.filter(filter).foldLeft((state.needles(MainBed).toMap, state.needles(DoubleBed).toMap)) {
      case ((main, double), fromN) => movementTarget(fromN).map { toN =>
        if (main(fromN).position.isWorking) {
          val m = NeedleState(NeedleB)
          val d = NeedleState(NeedleB, double(toN).yarn ++ main(fromN).yarn)
          (main + (fromN -> m), double + (toN -> d))
        } else (main, double)
      }.getOrElse(main, double)
    }
    state.modifyNeedles(MainBed, nm).modifyNeedles(DoubleBed, nd).success
  }
  def movementTarget(from: Needle) = {
    val index = flip match {
      case Some(flipAt) => flipAt.index + flipAt.index - from.index + offset
      case None => from.index + offset
    }
    Some(index).filter(i => i >= 0 && i < Needle.count).map(Needle.atIndex)
  }
}

/** Transfer the yarn from the double bed to the main bed. Affected needles are moved to B position. */
case class MoveToMainBed(filter: Needle => Boolean, offset: Int = 0) extends Step {
  override def apply(state: KnittingState) = {
    val working = state.workingNeedles(MainBed).contains _
    val (nm, nd) = Needle.all.
      filter(filter).
      filter(working).
      foldLeft((state.needles(MainBed).toMap, state.needles(DoubleBed).toMap)) {
      case ((main, double), doubleNeedle) =>
        val mainNeedle = doubleNeedle + offset
        val m = main(mainNeedle)
        (main + (mainNeedle -> NeedleState(NeedleB, m.yarn ++ double(doubleNeedle).yarn)),
          double + (doubleNeedle -> NeedleState(NeedleB)))
    }
    state.modifyNeedles(MainBed, nm).modifyNeedles(DoubleBed, nd).success
  }
}

case class AddCarriage(carriage: Carriage, at: LeftRight = Left) extends Step {
  override def apply(state: KnittingState) = Try {
    require(state.carriageState(carriage).position == CarriageRemoved,
      s"Can only add removed $carriage-carriage")
    state.moveCarriage(carriage, at)
  }.toSuccess
}

/**
 * Moves the needle into A position and moves the yarns that were on in one needle in the
 * given direction. The needle the yarn is moved to is left in the B position.
 */
case class RetireNeedle(bed: Bed, at: Needle, direction: Direction) extends Step {
  override def apply(state: KnittingState) = Try {
    require(at != Needle.all.head || direction == ToRight, "Cannot move to the left on first needle")
    require(at != Needle.all.last || direction == ToLeft, "Cannot move to the right on last needle")
    val needles = state.needles(bed)
    val before = needles(at)
    if (before.position.isWorking) {
      val m2 = NeedleState(NeedleB, needles(target).yarn ++ before.yarn)
      state.modifyNeedles(bed, needles.toMap + (target -> m2) + (at -> NeedleState(NeedleA)))
    } else state
  }.toSuccess
  lazy val target = at + (if (direction == ToLeft) -1 else 1)
}

/** Retire needles with the double decker. */
case class RetireWithDouble(bed: Bed, leftmost: Needle, direction: Direction) extends Step {
  override def apply(state: KnittingState) = {
    val composite = direction match {
      case ToLeft =>
        RetireNeedle(bed, leftmost, ToLeft) andThen
          RetireNeedle(bed, leftmost + 1, ToLeft)
      case ToRight =>
        RetireNeedle(bed, leftmost + 1, ToRight) andThen
          RetireNeedle(bed, leftmost, ToRight)
    }
    composite(state)
  }
  def affectedNeedles = direction match {
    case ToLeft => Set(leftmost, leftmost + 1, leftmost - 1)
    case ToRight => Set(leftmost, leftmost + 1, leftmost + 2)
  }
}

/** Retire needles with the triple decker. */
case class RetireWithTriple(bed: Bed, leftmost: Needle, direction: Direction) extends Step {
  override def apply(state: KnittingState) = {
    val composite = direction match {
      case ToLeft =>
        RetireNeedle(bed, leftmost, ToLeft) andThen
          RetireNeedle(bed, leftmost + 1, ToLeft) andThen
          RetireNeedle(bed, leftmost + 2, ToLeft)
      case ToRight =>
        RetireNeedle(bed, leftmost + 2, ToRight) andThen
          RetireNeedle(bed, leftmost + 1, ToRight) andThen
          RetireNeedle(bed, leftmost, ToRight)
    }
    composite(state)
  }
  def affectedNeedles = direction match {
    case ToLeft => Set(leftmost, leftmost + 1, leftmost + 2, leftmost - 1)
    case ToRight => Set(leftmost, leftmost + 1, leftmost + 2, leftmost + 3)
  }
}

case class HangOnCastOnComb() extends Step with NonOptimizable {
  override def apply(state: KnittingState) = state.success
}