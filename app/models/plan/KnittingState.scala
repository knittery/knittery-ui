package models.plan

import scalaz._
import Scalaz._
import models._

case class KnittingState(needles: NeedleStateRow,
  carriageState: CarriageStates,
  output: Knitted) {

  def workingNeedles = Needle.all.filter(needles(_).position.isWorking)
  def nextDirection(carriage: Carriage) = carriageState.get(carriage).map(_.position) match {
    case Some(pos) =>
      val overlappedWorking = carriage.over(pos).filter(needles(_).position.isWorking)
      if (overlappedWorking.nonEmpty) s"carriage still over working needeles ${overlappedWorking.mkString(",")}".fail
      else pos.directionTo(workingNeedles.headOption.getOrElse(Needle.middle)).success
    case None => ToRight.success
  }

  def moveNeedles(positions: Needle => NeedlePosition) =
    modifyNeedles(Needle.all.map(n => n -> needles(n).copy(position = positions(n))).toMap)
  def modifyNeedles(newNeedles: NeedleStateRow) = copy(needles = newNeedles)

  def knit(f: Needle => Stitch) = copy(output = output + f)
}
object KnittingState {
  val initial = KnittingState(_ => NeedleState(NeedleA), CarriageStates.empty, Knitted.empty)
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
}
object CarriageStates {
  def empty = new CarriageStates {
    override val data = Map.empty
  }
}