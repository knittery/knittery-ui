package models.guide

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import akka.actor._
import akka.pattern.{ask,pipe}
import akka.util.Timeout
import ch.inventsoft.graph.layout.Layout
import models.plan._
import models.machine._
import utils.SubscriptionActor

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

  /** Get 3D Layout as soon as ready. Answer: Knitted3DLayout. */
  case object GetKnitted3D extends Command
  case class Knitted3DLayout(knitted: Knitted3D, layout: Layout[Stitch3D]) extends Event

  /** Subscribe to ChangeEvent. Answer: Command[Not]Executed. */
  case object Subscribe extends Command
  case class ChangeEvent(newStep: GuideStep) extends Event
  /** Unsubscribe from ChangeEvent. Answer: Command[Not]Executed. */
  case object Unsubscribe extends Command

  def props(machine: ActorRef) = Props(new Guider(machine))

  def subscription(guider: ActorRef) = Props(new SubscriptionActor {
    val to = guider
    def subscribe = Subscribe
    def subscribed = {
      case CommandExecuted(Subscribe) => true
      case CommandNotExecuted(Subscribe, _) => false
    }
    def unsubscribe = Unsubscribe
  })

  private class Guider(machine: ActorRef) extends Actor {
    context.actorOf(Machine.subscription(machine))

    override def receive = {
      case cmd @ LoadPlan(plan) =>
        val step = GuideStep(plan)
        val output3D = step.last.stateAfter.output3D
        val layouter = context actorOf Layouter.props(output3D)
        context become stepping(step, layouter)
        sender ! CommandExecuted(cmd)
    }

    var subscribers = Set.empty[ActorRef]
    def notify(msg: Any) = subscribers.foreach(_ ! msg)

    def changeStep(newStep: GuideStep, layouter: ActorRef) = {
      notify(ChangeEvent(newStep))
      newStep.step match {
        case KnitRow(_, _, pattern) =>
          machine ! Machine.LoadNeedlePattern(pattern)
        case _ => ()
      }
      context become stepping(newStep, layouter)
    }

    private var row = 0

    def stepping(step: GuideStep, layouter: ActorRef): Receive = receive orElse {
      case QueryStep => sender ! CurrentStep(step)

      case GetKnitted3D =>
        implicit val timeout: Timeout = 10.minutes
        (layouter ? Layouter.Get).map {
          case layout: Layout[Stitch3D] =>
            Knitted3DLayout(step.last.stateAfter.output3D, layout)
        }.pipeTo(sender)

      case Machine.PositionChanged(_, _, old) if old == row =>
        ()
      case Machine.PositionChanged(_, _, r) =>
        row = r
        if (!step.isLast) {
          val newStep = step.allFromHere.tail.
            find(_.isKnitting).getOrElse(step.last)
          changeStep(newStep, layouter)
        }

      case cmd @ Next =>
        if (!step.isLast) {
          changeStep(step.next, layouter)
          sender ! CommandExecuted(cmd)
        } else {
          sender ! CommandNotExecuted(cmd, "Already at last step")
        }
      case cmd @ Previous =>
        if (!step.isFirst) {
          changeStep(step.previous, layouter)
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