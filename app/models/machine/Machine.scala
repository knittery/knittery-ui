package models.machine

import akka.actor._
import models._
import models.connector.Connector._
import utils.SubscriptionActor

/**
 * Actor representing the knitting machine.
 * - <i>connector</i> must send MachineEvents to the actor or crash if it loses the
 * connection to the machine.
 */
class Machine(connectorProps: Props) extends Actor {

  import Machine._

  override def supervisorStrategy = OneForOneStrategy() {
    case _ => SupervisorStrategy.Restart
  }

  val connector = context.actorOf(connectorProps, "connector")
  val rowTracker = context.actorOf(RowTracker.props(self))

  var subscribers = Set.empty[ActorRef]
  def notify(msg: Event) =
    subscribers.foreach(_ ! msg)

  var positions = Map.empty[Carriage, CarriagePosition]
  var lastCarriage: Carriage = KCarriage
  var pattern = AllNeedlesToB

  override def receive = {
    case Subscribe =>
      subscribers += sender
      context watch sender
      sender ! Subscribed
    case Unsubscribe =>
      subscribers -= sender

    case GetPositions =>
      sender ! Positions(positions)

    case LoadNeedlePattern(patternRow) =>
      connector ! LoadPatternRow(patternRow)
      notify(NeedlePatternUpdate(patternRow))

    case GetNeedlePattern =>
      sender ! NeedlePatternUpdate(pattern)

    case SetWorkingZone(from, until) =>
      rowTracker ! RowTracker.WorkingZone(from, until)
      sender ! WorkingZoneSet(from, until)

    //MachineEvents
    case pu@PositionUpdate(pos, direction, Some(carriage)) =>
      rowTracker ! pu
      notify(PositionChanged(carriage, pos))
      positions += carriage -> pos

    //Events from subactors
    case RowTracker.NextRow(carriage) =>
      notify(Machine.NextRow(carriage))

    case Terminated(`connector`) => //Connector crashed
      //TODO handle the crash
      //TODO reset the positions and requery somehow?
      ()
    case Terminated(subscriber) =>
      subscribers -= subscriber
  }
}

object Machine {
  def props(connector: Props) = Props(new Machine(connector))

  def subscription(machine: ActorRef) = Props(new SubscriptionActor {
    val to = machine
    def subscribe = Subscribe
    def subscribed = {
      case Subscribed => true
    }
    def unsubscribe = Unsubscribe
  })

  sealed trait Command
  sealed trait Event

  case object Subscribe extends Command
  case object Unsubscribe extends Command
  case object Subscribed extends Event
  sealed trait Notification extends Event

  case class PositionChanged(carriage: Carriage, position: CarriagePosition) extends Notification
  case class NeedlePatternUpdate(currentRow: NeedleActionRow) extends Notification
  case class NextRow(carriage: Carriage) extends Notification

  case object GetPositions extends Command
  case class Positions(positions: Map[Carriage, CarriagePosition]) extends Event with Notification

  case class LoadNeedlePattern(pattern: NeedleActionRow) extends Command
  case class NeedlePatternLoaded(pattern: NeedleActionRow) extends Event

  case class SetWorkingZone(from: Needle, until: Needle) extends Command
  case class WorkingZoneSet(from: Needle, until: Needle) extends Event

  /** Will be answered by a NeedlePatternUpdate. */
  case object GetNeedlePattern extends Command
}