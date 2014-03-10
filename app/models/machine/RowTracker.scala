package models.machine

import akka.actor._
import models._
import models.connector.Connector.PositionUpdate

/** Keeps track of the current row number. */
private object RowTracker {
  case class SetRow(row: Int)
  val ResetRow = SetRow(0)
  case class RowChanged(row: Int)

  def props(commander: ActorRef) = Props(new RowTracker(commander))

  private class RowTracker(commander: ActorRef) extends Actor {
    context watch commander
    var row: Int = -1

    override def receive = general orElse {
      case PositionUpdate(CarriageOverNeedles(needle), direction, _) =>
        context.become(verifyDirection(direction, needle), discardOld = false)
    }

    def knitting(direction: Direction): Receive = general orElse {
      case PositionUpdate(CarriageOverNeedles(needle), dir, _) if dir != direction =>
        context.become(verifyDirection(dir, needle), discardOld = false)
    }

    def verifyDirection(candidate: Direction, startAt: Needle): Receive = general orElse {
      case PositionUpdate(CarriageOverNeedles(needle), `candidate`, _) =>
        val distance = (needle.index - startAt.index) * (if (candidate == ToLeft) -1 else 1)
        if (distance < 0 || distance > 30) context.unbecome()
        else if (distance > 5) {
          context.unbecome() // so we don't leak memory
          nextRow()
          context become knitting(candidate)
        }
      case PositionUpdate(CarriageOverNeedles(_), _, _) =>
        context.unbecome()
    }

    def general: Receive = {
      case SetRow(r) =>
        row = r
        commander ! RowChanged(row)
    }

    def nextRow() = {
      row = row + 1
      commander ! RowChanged(row)
    }
  }
}
