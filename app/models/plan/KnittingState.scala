package models.plan

import scalaz._
import Scalaz._
import models._

case class KnittingState(
  bedNeedles: Map[Bed, Map[Needle, NeedleState]],
  carriageState: CarriageStates,
  output: Knitted,
  output3D: Knitted3D,
  yarnAttachments: Map[YarnPiece, YarnAttachment]) {

  def nextDirection(carriage: Carriage) = carriageState(carriage).position match {
    case CarriageRemoved => "Cannot find direction for removed carriage".fail
    case pos =>
      val working = this.workingNeedles.toSet
      val overlappedWorking = carriage.over(pos).filter(working.contains)
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

  def needles(bed: Bed): NeedleStateRow = bedNeedles.getOrElse(bed, _ => NeedleState(NeedleA))
  def workingNeedles: Seq[Needle] = {
    Beds.all.map(workingNeedles).
      foldLeft(Set.empty[Needle])(_ ++ _).
      toSeq.sorted
  }
  def workingNeedles(bed: Bed): Seq[Needle] =
    Needle.all.filter(n => needles(bed)(n).position.isWorking)

  def moveNeedles(bed: Bed, positions: Needle => NeedlePosition) = {
    def value(n: Needle) = n -> needles(bed)(n).copy(position = positions(n))
    modifyNeedles(bed, Needle.all.map(value).toMap)
  }
  def modifyNeedles(bed: Bed, newNeedles: NeedleStateRow) =
    copy(bedNeedles = bedNeedles + (bed -> newNeedles.toMap))

  def knit(f: Needle => Stitch) = copy(output = output + f)

  def pushRow(forNeedles: Needle => Boolean) = {
    val changed = yarnAttachments.collect {
      case (yarn, ya) if forNeedles(ya.needle) =>
        (yarn, ya.copy(rowDistance = ya.rowDistance + 1))
    }
    copy(yarnAttachments = yarnAttachments ++ changed, output3D = output3D.pushDown)
  }
  def attachYarn(ya: YarnAttachment) = copy(yarnAttachments = yarnAttachments + (ya.yarn.start -> ya))
  def detachYarn(yarn: YarnPiece) = copy(yarnAttachments = yarnAttachments - yarn)

  def knit2(f: Knitted3D => Knitted3D) = copy(output3D = f(output3D))
}
object KnittingState {
  val initial = KnittingState(Map.empty, CarriageStates.empty, Knitted.empty, Knitted3D.empty, Map.empty)
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
  override def toString = data.toString
}
object CarriageStates {
  def empty = new CarriageStates {
    override val data = Map.empty[Carriage, CarriageState]
  }
}

case class YarnAttachment(yarn: YarnFlow, needle: Needle, bed: Bed, rowDistance: Int = 0)