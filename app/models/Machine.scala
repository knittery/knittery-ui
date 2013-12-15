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

  val connector = context.actorOf(connectorProps, "connector")
  context watch connector
  override def supervisorStrategy = OneForOneStrategy() {
    case _ => SupervisorStrategy.Restart
  }

  var subscribers = Set.empty[ActorRef]
  def notify(msg: Any) = subscribers.foreach(_ ! msg)

  var positions = Map.empty[CarriageType, CarriagePosition]

  override def receive = {
    case Subscribe =>
      subscribers += sender
      context watch sender
      sender ! Subscribed
    case Unsubscribe =>
      subscribers -= sender

    case GetPositions =>
      sender ! Positions(positions)

    //MachineEvents
    case PositionUpdate(pos, direction, Some(carriage)) =>
      notify(PositionChanged(carriage, pos))
      positions += carriage -> pos
    case PositionUpdate(_, _, _) => () //ignore

    case Terminated if sender == connector => //Connector crashed
      //TODO handle the crash
      //TODO reset the positions and requery somehow?
      ()
    case Terminated => // subscriber terminates
      subscribers -= sender
  }

}

object Machine {
  sealed trait Command
  sealed trait Event

  case object Subscribe extends Command
  case object Unsubscribe extends Command

  case object Subscribed extends Event
  case class PositionChanged(carriage: CarriageType, position: CarriagePosition) extends Event

  case object GetPositions extends Command
  case class Positions(positions: Map[CarriageType, CarriagePosition]) extends Event
}