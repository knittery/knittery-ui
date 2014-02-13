package models.plan

import scalaz._
import Scalaz._
import models._

case class KnittingState(
  needles: Map[Needle, NeedleState],
  doubleBedNeedles: Map[Needle, NeedleState],
  carriageState: CarriageStates,
  output: Knitted,
  output2: Knitted2,
  yarnAttachments: Map[YarnPiece, YarnAttachment]) {

  def workingNeedles = Needle.all.filter(needles(_).position.isWorking)
  def nextDirection(carriage: Carriage) = carriageState(carriage).position match {
    case CarriageRemoved => "Cannot find direction for removed carriage".fail
    case pos =>
      val overlappedWorking = carriage.over(pos).filter(needles(_).position.isWorking)
      if (overlappedWorking.nonEmpty) s"carriage still over working needeles ${overlappedWorking.mkString(",")}".fail
      else pos.directionTo(workingNeedles.headOption.getOrElse(Needle.middle)).success
  }

  def modifyCarriage(state: CarriageState) =
    copy(carriageState = carriageState + state)
  def moveCarriage(carriage: Carriage, in: Direction): KnittingState =
    moveCarriage(carriage, if (in == ToLeft) CarriageLeft(0) else CarriageRight(0))
  def moveCarriage(carriage: Carriage, to: LeftRight): KnittingState =
    moveCarriage(carriage, if (to == Left) CarriageLeft(0) else CarriageRight(0))
  def moveCarriage(carriage: Carriage, to: CarriagePosition) = modifyCarriage(carriage match {
    case KCarriage => carriageState(KCarriage).copy(position = to)
    case LCarriage => carriageState(LCarriage).copy(position = to)
    case GCarriage => carriageState(GCarriage).copy(position = to)
  })

  def moveNeedles(positions: Needle => NeedlePosition) =
    modifyNeedles(Needle.all.map(n => n -> needles(n).copy(position = positions(n))).toMap)
  def modifyNeedles(newNeedles: NeedleStateRow) = copy(needles = newNeedles.toMap)
  def moveDoubleBedNeedles(positions: Needle => NeedlePosition) =
    modifyDoubleBedNeedles(Needle.all.map(n => n -> needles(n).copy(position = positions(n))).toMap)
  def modifyDoubleBedNeedles(newNeedles: NeedleStateRow) = copy(doubleBedNeedles = newNeedles.toMap)

  def knit(f: Needle => Stitch) = copy(output = output + f)

  def pushRow(forNeedles: Needle => Boolean) = {
    val changed = yarnAttachments.collect {
      case (yarn, ya) if forNeedles(ya.needle) =>
        (yarn, ya.copy(rowDistance = ya.rowDistance + 1))
    }
    copy(yarnAttachments = yarnAttachments ++ changed)
  }
  def attachYarn(ya: YarnAttachment) = copy(yarnAttachments = yarnAttachments + (ya.yarn.start -> ya))
  def detachYarn(yarn: YarnPiece) = copy(yarnAttachments = yarnAttachments - yarn)

  def knit2(f: Knitted2 => Knitted2) = copy(output2 = f(output2))
}
object KnittingState {
  val initial = KnittingState((allNeedlesA _).toMap, (allNeedlesA _).toMap, CarriageStates.empty, Knitted.empty, Knitted2.empty, Map.empty)
  private def allNeedlesA(n: Needle) = NeedleState(NeedleA)
}

sealed trait CarriageStates {
  protected val data: Map[Carriage, CarriageState]
  def +(state: CarriageState) = {
    val data2 = data + (carriageFor(state) -> state)
    new CarriageStates {
      override val data = data2
    }
  }
  def apply(carriage: Carriage) = {
    data.get(carriage).getOrElse(carriage.initialState).
      asInstanceOf[carriage.State]
  }

  private def carriageFor(state: CarriageState) = state match {
    case _: KCarriage.State => KCarriage
    case _: LCarriage.State => LCarriage
    case _: GCarriage.State => GCarriage
  }

  override def hashCode = data.hashCode
  override def equals(o: Any) = o match {
    case o: CarriageStates => data == o.data
    case _ => false
  }
}
object CarriageStates {
  def empty = new CarriageStates {
    override val data = Map.empty[Carriage, CarriageState]
  }
}

case class YarnAttachment(yarn: YarnFlow, needle: Needle, mainBed: Boolean, rowDistance: Int = 0)