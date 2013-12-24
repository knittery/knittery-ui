package models

import akka.actor._
import connector.Connector.PositionUpdate

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

  def patternUpdate = {
    val prow =
      if (row < 0 || row >= pattern.height) NeedlePattern.empty.apply(0) _
      else pattern.apply(row) _
    NeedlePatternUpdate(prow, pattern)
  }

  var positions = Map.empty[CarriageType, CarriagePosition]
  var lastCarriage: CarriageType = KCarriage
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
      row = -1
      notify(patternUpdate)

    case GetNeedlePattern =>
      sender ! patternUpdate

    //MachineEvents
    case pu @ PositionUpdate(pos, direction, Some(carriage)) =>
      rowTracker ! pu
      notify(PositionChanged(carriage, pos, row))
      positions += carriage -> pos

    //Events from subactors
    case RowChanged(r) =>
      row = r
      notify(PositionChanged(lastCarriage, positions.get(lastCarriage).getOrElse(CarriageLeft(0)),
        row))
      notify(patternUpdate)

    case Terminated if sender == connector => //Connector crashed
      //TODO handle the crash
      //TODO reset the positions and requery somehow?
      ()
    case Terminated => // subscriber terminates
      subscribers -= sender
  }
}

object Machine {
  def props(connector: Props) = Props(new Machine(connector))

  sealed trait Command
  sealed trait Event

  case object Subscribe extends Command
  case object Unsubscribe extends Command

  case object Subscribed extends Event
  case class PositionChanged(carriage: CarriageType, position: CarriagePosition, row: Int) extends Event
  case class NeedlePatternUpdate(currentRow: NeedlePatternRow, pattern: NeedlePattern) extends Event

  case object GetPositions extends Command
  case class Positions(positions: Map[CarriageType, CarriagePosition], row: Int) extends Event

  case class LoadPattern(pattern: NeedlePattern) extends Command
  case class PatternLoaded(pattern: NeedlePattern) extends Event

  /** Will be answered by a NeedlePatternUpdate. */
  case object GetNeedlePattern extends Command
}