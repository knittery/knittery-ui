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

  override def receive = {
    case Subscribe =>
      subscribers += sender
      context watch sender
    case Unsubscribe =>
      subscribers -= sender

    //MachineEvents
    case PositionUpdate(pos, direction, carriage) =>
      ???

    case Terminated if sender == connector => //Connector crashed
      //TODO handle the crash
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
}