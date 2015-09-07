package knit.plan2

import scala.language.higherKinds
import scalaz._
import Scalaz._
import knit._

object KnittingPlan {
  import KnittingPlanOps._

  //cannot use FreeC, because type inference for implicits will diverge
  type KnittingPlan[A] = Free[KnittingPlanF, A]

  /** Alias for value.pure[KnittingPlan] */
  def value[A](value: A): KnittingPlan[A] = value.pure[KnittingPlan]

  /** Knits a row using the specified carriage. */
  def knitRow(carriage: Carriage, direction: Direction, needlesTo: NeedleActionRow = AllNeedlesToB): KnittingPlan[Unit] =
    Free.liftFC(KnitRow(carriage, direction, needlesTo))

  /** Manual movement of needles. */
  def moveNeedles(bed: Bed, to: NeedlePatternRow): KnittingPlan[Unit] = Free.liftFC(MoveNeedles(bed, to))

  /**
   * Moves the needle into A position and moves the yarns that were on in one needle in the
   * given direction. The needle the yarn is moved to is left in the B position.
   */
  def retireNeedle(bed: Bed, at: Needle, direction: Direction): KnittingPlan[Unit] = Free
    .liftFC(RetireNeedle(bed, at, direction))

  /** Retire needles with the double decker. */
  def retireWithDouble(bed: Bed, leftmost: Needle, direction: Direction): KnittingPlan[Unit] =
    Free.liftFC(RetireWithDouble(bed, leftmost, direction))

  /** Retire needles with the triple decker. */
  def retireWithTriple(bed: Bed, leftmost: Needle, direction: Direction): KnittingPlan[Unit] =
    Free.liftFC(RetireWithTriple(bed, leftmost, direction))

  def addCarriage(carriage: Carriage, at: LeftRight = Left): KnittingPlan[Unit] =
    Free.liftFC(AddCarriage(carriage, at))
  def changeKCarriageSettings(settings: KCarriage.Settings, assembly: KCarriage.Assembly): KnittingPlan[Unit] =
    Free.liftFC(ChangeKCarriageSettings(settings, assembly))
  def changeLCarriageSettings(settings: LCarriage.Settings): KnittingPlan[Unit] =
    Free.liftFC(ChangeLCarriageSettings(settings))
  def changeGCarriageSettings(settings: GCarriage.Settings): KnittingPlan[Unit] =
    Free.liftFC(ChangeGCarriageSettings(settings))

  def threadYarnK(yarnA: Option[YarnPiece], yarnB: Option[YarnPiece]): KnittingPlan[Unit] =
    Free.liftFC(ThreadYarnK(yarnA, yarnB))
  def threadYarnG(yarn: Option[YarnPiece]): KnittingPlan[Unit] =
    Free.liftFC(ThreadYarnG(yarn))

  /**
   * Performs a closed cast on for the needles. The needles are then moved to D position.
   * All other needles are not touched.
   */
  def closedCastOn(bed: Bed, from: Needle, until: Needle, yarn: YarnPiece): KnittingPlan[Unit] =
    Free.liftFC(ClosedCastOn(bed, from, until, yarn))
  def closedCastOff(bed: Bed, withYarn: YarnPiece, filter: Needle => Boolean): KnittingPlan[Unit] =
    Free.liftFC(ClosedCastOff(bed, withYarn, filter))

  /** Moves the yarn from the main to the double bed. Needles affected are moved to B position. */
  def moveToDoubleBed(filter: Needle => Boolean, offset: Int = 0, flip: Option[Needle] = None): KnittingPlan[Unit] =
    Free.liftFC(MoveToDoubleBed(filter, offset, flip))
  /** Transfer the yarn from the double bed to the main bed. Affected needles are moved to B position. */
  def moveToMainBed(filter: Needle => Boolean, offset: Int = 0): KnittingPlan[Unit] =
    Free.liftFC(MoveToMainBed(filter, offset))

  def hangOnCastOnComb(): KnittingPlan[Unit] = Free.liftFC(HangOnCastOnComb)

  def carriagePosition(carriage: Carriage): KnittingPlan[CarriagePosition] = Free.liftFC(GetCarriagePosition(carriage))

  def needlePositions(bed: Bed): KnittingPlan[NeedlePatternRow] = Free.liftFC(GetNeedlePositions(bed))

  /** Fail the knitting because of an error. */
  def knittingError[A](error: String): KnittingPlan[A] = Free.liftFC(KnittingError(error))

  /** Convert a String \/ into a knitting plan action. */
  implicit object fromDisjunction extends (({type λ[A] = String \/ A})#λ ~> KnittingPlan) {
    def apply[A](fa: String \/ A) = fa.fold[KnittingPlan[A]](knittingError, value)
  }
  def valueOrError[A](v: String \/ A) = fromDisjunction(v)


  // Functions for the plan monad
  def flatten[A[_], B](plan: KnittingPlan[A[B]])(implicit t: A ~> KnittingPlan): KnittingPlan[B] =
    plan.flatMap(t.apply)
  implicit class KnittingPlanFlattenOp[A[_], B](plan: KnittingPlan[A[B]]) {
    def flatten(implicit t: A ~> KnittingPlan): KnittingPlan[B] = KnittingPlan.flatten(plan)
  }
  // to help the compiler that has problems finding out that from disjunction matches..
  def flatten[A](plan: KnittingPlan[String \/ A]): KnittingPlan[A] = plan.flatMap(fromDisjunction.apply)
  implicit class KnittingPlanFlattenDisjunctionOp[A](plan: KnittingPlan[String \/ A]) {
    def flatten: KnittingPlan[A] = KnittingPlan.flatten(plan)
  }


  object KnittingPlanOps {
    sealed trait KnittingPlanOp[+A]
    type KnittingPlanF[A] = Coyoneda[KnittingPlanOp, A]

    /** Knits a row using a carriage. */
    case class KnitRow(carriage: Carriage, direction: Direction, pattern: NeedleActionRow = AllNeedlesToB)
      extends KnittingPlanOp[Unit]

    /** Manual movement of needles. */
    case class MoveNeedles(bed: Bed, to: NeedlePatternRow) extends KnittingPlanOp[Unit]
    /**
     * Moves the needle into A position and moves the yarns that were on in one needle in the
     * given direction. The needle the yarn is moved to is left in the B position.
     */
    case class RetireNeedle(bed: Bed, at: Needle, direction: Direction) extends KnittingPlanOp[Unit]
    /** Retire needles with the double decker. */
    case class RetireWithDouble(bed: Bed, leftmost: Needle, direction: Direction) extends KnittingPlanOp[Unit]
    /** Retire needles with the triple decker. */
    case class RetireWithTriple(bed: Bed, leftmost: Needle, direction: Direction) extends KnittingPlanOp[Unit]

    case class AddCarriage(carriage: Carriage, at: LeftRight = Left) extends KnittingPlanOp[Unit]
    case class ChangeKCarriageSettings(settings: KCarriage.Settings, assembly: KCarriage.Assembly)
      extends KnittingPlanOp[Unit]
    case class ChangeLCarriageSettings(settings: LCarriage.Settings) extends KnittingPlanOp[Unit]
    case class ChangeGCarriageSettings(settings: GCarriage.Settings) extends KnittingPlanOp[Unit]

    case class ThreadYarnK(yarnA: Option[YarnPiece], yarnB: Option[YarnPiece]) extends KnittingPlanOp[Unit]
    case class ThreadYarnG(yarn: Option[YarnPiece]) extends KnittingPlanOp[Unit]

    /**
     * Performs a closed cast on for the needles. The needles are then moved to D position.
     * All other needles are not touched.
     */
    case class ClosedCastOn(bed: Bed, from: Needle, until: Needle, yarn: YarnPiece) extends KnittingPlanOp[Unit]
    case class ClosedCastOff(bed: Bed, withYarn: YarnPiece, filter: Needle => Boolean) extends KnittingPlanOp[Unit]

    /** Moves the yarn from the main to the double bed. Needles affected are moved to B position. */
    case class MoveToDoubleBed(filter: Needle => Boolean, offset: Int = 0, flip: Option[Needle] = None)
      extends KnittingPlanOp[Unit]
    /** Transfer the yarn from the double bed to the main bed. Affected needles are moved to B position. */
    case class MoveToMainBed(filter: Needle => Boolean, offset: Int = 0) extends KnittingPlanOp[Unit]

    case object HangOnCastOnComb extends KnittingPlanOp[Unit]

    case class GetCarriagePosition(carriage: Carriage) extends KnittingPlanOp[CarriagePosition]

    case class GetNeedlePositions(bed: Bed) extends KnittingPlanOp[NeedlePatternRow]

    case class KnittingError(error: String) extends KnittingPlanOp[Nothing]
  }
}
