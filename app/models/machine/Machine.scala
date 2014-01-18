package models.machine

import akka.actor._
import models._
import models.connector.Connector._
import NeedlePatternKnitter.NeedlePatternRowChanged
import NeedlePatternKnitter.SetNeedlePattern
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
  import NeedlePatternKnitter._

  override def supervisorStrategy = OneForOneStrategy() {
    case _ => SupervisorStrategy.Restart
  }

  val connector = context.actorOf(connectorProps, "connector")
  val rowTracker = context.actorOf(RowTracker.props(self))
  val patternKnitter = context.actorOf(NeedlePatternKnitter.props(self, connector))

  var subscribers = Set.empty[ActorRef]
  def notify(msg: Event) =
    subscribers.foreach(_ ! msg)

  var positions = Map.empty[Carriage, CarriagePosition]
  var lastCarriage: Carriage = KCarriage
  var row: Int = -1
  var pattern = NeedlePattern.empty

  override def receive = {
    case Subscribe =>
      subscribers += sender
      context watch sender
      sender ! Subscribed
    case Unsubscribe =>
      subscribers -= sender

    case GetPositions =>
      sender ! Positions(positions, row)

    case LoadPattern(p) =>
      pattern = p
      rowTracker ! SetRow(-1)
      patternKnitter ! SetNeedlePattern(p, -1)

    case GetNeedlePattern =>
      sender ! NeedlePatternUpdate(pattern.orElse(NeedlePattern.empty)(row), pattern)

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
      patternKnitter ! event
    case NeedlePatternRowChanged(rp, _, pattern) =>
      notify(NeedlePatternUpdate(rp, pattern))

    case Terminated(`patternKnitter`) => //Pattern knitter crashed
      patternKnitter ! SetNeedlePattern(pattern, row)
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
  case class NeedlePatternUpdate(currentRow: NeedlePatternRow, pattern: NeedlePattern) extends Event

  case object GetPositions extends Command
  case class Positions(positions: Map[Carriage, CarriagePosition], row: Int) extends Event

  case class LoadPattern(pattern: NeedlePattern) extends Command
  case class PatternLoaded(pattern: NeedlePattern) extends Event

  /** Will be answered by a NeedlePatternUpdate. */
  case object GetNeedlePattern extends Command
}