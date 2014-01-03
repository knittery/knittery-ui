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

  case object Next extends Command
  case object Previous extends Command

  case object QueryStep extends Command
  case class CurrentStep(step: GuideStep) extends Event

  private class Guider(plan: Plan) extends Actor {
    override def preStart = {
      self ! GuideStep(plan)
    }
    override def receive = {
      case step: GuideStep => context become stepping(step)
    }

    def stepping(step: GuideStep): Receive = {
      case QueryStep => sender ! CurrentStep(step)

      case Next =>
        if (!step.isLast) {
          context become (stepping(step.next))
          sender ! CommandExecuted(Next)
        } else {
          sender ! CommandNotExecuted(Next, "Already at last step")
        }
      case Previous =>
        if (!step.isFirst) {
          context become (stepping(step.previous))
          sender ! CommandExecuted(Previous)
        } else {
          sender ! CommandNotExecuted(Previous, "Already at first step")
        }
    }
  }

}