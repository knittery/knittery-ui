package models

import akka.actor._
import connector.PositionUpdate

/**
 * Actor representing the knitting machine.
 * - <i>connector</i> must send MachineEvents to the actor or crash if it loses the
 *   connection to the machine.
 */
class Machine(connectorProps: Props) extends Actor {
  import Machine._
  import RowTracker._

  override def supervisorStrategy = OneForOneStrategy() {
    case _ => SupervisorStrategy.Restart
  }

  val connector = context.actorOf(connectorProps, "connector")
  val rowTracker = context.actorOf(RowTracker.props(self))

  var subscribers = Set.empty[ActorRef]
  def notify(msg: Event) =
    subscribers.foreach(_ ! msg)

  var positions = Map.empty[CarriageType, CarriagePosition]
  var row: Int = -1

  override def receive = {
    case Subscribe =>
      subscribers += sender
      context watch sender
      sender ! Subscribed
    case Unsubscribe =>
      subscribers -= sender

    case GetPositions =>
      sender ! Positions(positions)
    case GetKnittingStatus =>
      sender ! KnittingStatus(row)

    //MachineEvents
    case pu @ PositionUpdate(pos, direction, Some(carriage)) =>
      rowTracker ! pu
      notify(PositionChanged(carriage, pos))
      positions += carriage -> pos

    //Events from subactors
    case RowChanged(r) =>
      row = r
      notify(KnittingEvent(row))

    case Terminated if sender == connector => //Connector crashed
      //TODO handle the crash
      //TODO reset the positions and requery somehow?
      ()
    case Terminated => // subscriber terminates
      subscribers -= sender
  }
}

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
        context.become(verifyDirection(direction, needle), false)
    }

    def knitting(direction: Direction): Receive = general orElse {
      case PositionUpdate(CarriageOverNeedles(needle), dir, _) if dir != direction =>
        context.become(verifyDirection(dir, needle), false)
    }

    def verifyDirection(candidate: Direction, startAt: Needle): Receive = general orElse {
      case PositionUpdate(CarriageOverNeedles(needle), `candidate`, _) =>
        val distance = (needle.index - startAt.index) * (if (candidate == Left) -1 else 1)
        if (distance < 0 || distance > 30) context.unbecome
        else if (distance > 5) {
          context.unbecome // so we don't leak memory
          nextRow
          context become knitting(candidate)
        }
      case PositionUpdate(CarriageOverNeedles(_), _, _) =>
        context.unbecome
    }

    def general: Receive = {
      case SetRow(r) =>
        row = r
        commander ! RowChanged(row)
    }

    def nextRow = {
      row = row + 1
      commander ! RowChanged(row)
    }
  }
}

object Machine {
  def props(connector: Props) = Props(new Machine(connector))

  sealed trait Command
  sealed trait Event

  case object Subscribe extends Command
  case object Unsubscribe extends Command

  case object Subscribed extends Event
  case class PositionChanged(carriage: CarriageType, position: CarriagePosition) extends Event
  case class KnittingEvent(row: Int) extends Event

  case object GetPositions extends Command
  case class Positions(positions: Map[CarriageType, CarriagePosition]) extends Event

  case object GetKnittingStatus extends Command
  case class KnittingStatus(row: Int) extends Event
}