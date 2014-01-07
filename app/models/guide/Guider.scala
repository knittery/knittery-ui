package models.guide

import scalaz._
import Scalaz._
import akka.actor._
import models._
import models.plan._

/** An instance of a plan execution. Keeps track of the current step. */
object Guider {
  sealed trait Command
  sealed trait Event
  case class CommandExecuted(command: Command) extends Event
  case class CommandNotExecuted(command: Command, reason: String) extends Event

  /** Load a new plan. Answer: Command[Not]Executed. */
  case class LoadPlan(plan: Plan) extends Command

  /** Move to the next step. Answer: Command[Not]Executed. */
  case object Next extends Command
  /** Move to the previous step. Answer: Command[Not]Executed. */
  case object Previous extends Command

  /** Get the current step. Answer: CurrentStep. */
  case object QueryStep extends Command
  case class CurrentStep(step: GuideStep) extends Event

  /** Subscribe to ChangeEvent. Answer: Command[Not]Executed. */
  case object Subscribe extends Command
  case class ChangeEvent(newStep: GuideStep) extends Event
  /** Unsubscribe from ChangeEvent. Answer: Command[Not]Executed. */
  case object Unsubscribe extends Command

  def props = Props(new Guider)

  private class Guider extends Actor {
    override def receive = {
      case cmd @ LoadPlan(plan) =>
        val step = GuideStep(plan)
        context become (stepping(step))
        sender ! CommandExecuted(cmd)
    }

    var subscribers = Set.empty[ActorRef]
    def notify(msg: Any) = subscribers.foreach(_ ! msg)

    def changeStep(newStep: GuideStep) = {
      notify(ChangeEvent(newStep))
      context become (stepping(newStep))
    }

    def stepping(step: GuideStep): Receive = receive orElse {
      case QueryStep => sender ! CurrentStep(step)

      case cmd @ Next =>
        if (!step.isLast) {
          changeStep(step.next)
          sender ! CommandExecuted(cmd)
        } else {
          sender ! CommandNotExecuted(cmd, "Already at last step")
        }
      case cmd @ Previous =>
        if (!step.isFirst) {
          changeStep(step.previous)
          sender ! CommandExecuted(cmd)
        } else {
          sender ! CommandNotExecuted(cmd, "Already at first step")
        }

      case cmd @ Subscribe =>
        subscribers = subscribers + sender
        context watch sender
        sender ! CommandExecuted(cmd)
      case cmd @ Unsubscribe =>
        subscribers = subscribers - sender
        context unwatch sender
        sender ! CommandExecuted(cmd)
      case Terminated if subscribers.contains(sender) =>
        subscribers = subscribers - sender
    }
  }
}