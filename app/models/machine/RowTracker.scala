package models.machine

import akka.actor._
import models._
import models.connector.Connector.PositionUpdate

/** Keeps track of the current row number. */
private object RowTracker {
  case object NextRow
  case class WorkingZone(from: Needle, until: Needle) {
    private[RowTracker] def contains(pos: CarriagePosition) = pos match {
      case CarriageOverNeedles(n) => n >= from && n <= until
      case _ => false
    }
    private[RowTracker] def relative(pos: CarriagePosition) = pos match {
      case CarriageOverNeedles(n) => if (n < from) Left else Right
      case CarriageLeft(_) => Left
      case CarriageRight(_) => Right
      case CarriageRemoved => throw new IllegalStateException("carriage is removed")
    }
  }

  def props(commander: ActorRef) = Props(new RowTracker(commander))

  private object Position {
    def unapply(msg: Any): Option[(CarriagePosition, Direction)] = msg match {
      case PositionUpdate(pos, direction, _) => Some(pos, direction)
      case _ => None
    }
  }

  private class RowTracker(commander: ActorRef) extends Actor {
    var workingZone = WorkingZone(Needle.middle - 1, Needle.middle + 1)
    var lastPos: CarriagePosition = CarriageLeft(0)
    context watch commander

    def receive = {
      case Position(pos, dir) if workingZone.contains(pos) =>
        lastPos = pos
        context become track(dir)
      case Position(pos, _) =>
        lastPos = pos
        context become track(workingZone.relative(pos).direction.reverse)
      case w: WorkingZone =>
        workingZone = w
    }

    def track(direction: Direction): Receive = {
      case Position(pos, _) if !workingZone.contains(pos) && workingZone.relative(pos) == direction.towards =>
        commander ! NextRow
        lastPos = pos
        context become track(direction.reverse)
      case Position(pos, _) =>
        lastPos = pos
      case w: WorkingZone if workingZone != w =>
        workingZone = w
        if (!w.contains(lastPos))
          context become track(w.relative(lastPos).direction.reverse)
    }
  }
}
