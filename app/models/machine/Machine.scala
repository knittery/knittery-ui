package models.machine

import akka.actor._
import models._
import models.connector.Connector._
import RowTracker.RowChanged
import utils.SubscriptionActor

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

  var positions = Map.empty[Carriage, CarriagePosition]
  var lastCarriage: Carriage = KCarriage
  var row: Int = -1
  var pattern = AllNeedlesToB

  override def receive = {
    case Subscribe =>
      subscribers += sender
      context watch sender
      sender ! Subscribed
    case Unsubscribe =>
      subscribers -= sender

    case GetPositions =>
      sender ! Positions(positions, row)

    case LoadNeedlePattern(patternRow) =>
      connector ! LoadPatternRow(patternRow)
      notify(NeedlePatternUpdate(patternRow))

    case GetNeedlePattern =>
      sender ! NeedlePatternUpdate(pattern)

    //MachineEvents
    case pu @ PositionUpdate(pos, direction, Some(carriage)) =>
      rowTracker ! pu
      notify(PositionChanged(carriage, pos, row))
      positions += carriage -> pos

    //Events from subactors
    case event @ RowChanged(r) =>
      row = r
      notify(PositionChanged(lastCarriage, positions.get(lastCarriage).getOrElse(CarriageLeft(0)),
        row))

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
  case class PositionChanged(carriage: Carriage, position: CarriagePosition, row: Int) extends Event
  case class NeedlePatternUpdate(currentRow: NeedleActionRow) extends Event

  case object GetPositions extends Command
  case class Positions(positions: Map[Carriage, CarriagePosition], row: Int) extends Event

  case class LoadNeedlePattern(pattern: NeedleActionRow) extends Command
  case class NeeldePatternLoaded(pattern: NeedleActionRow) extends Event

  /** Will be answered by a NeedlePatternUpdate. */
  case object GetNeedlePattern extends Command
}