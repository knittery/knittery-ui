package models.guide

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import ch.inventsoft.graph.layout.Layout
import models._
import models.plan._
import models.machine.Machine
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
  case object NextStep extends Command
  /** Move to the previous step. Answer: Command[Not]Executed. */
  case object PreviousStep extends Command
  /** Move to the next instruction. Answer: Command[Not]Executed. */
  case object NextInstruction extends Command
  /** Move to the previous instruction. Answer: Command[Not]Executed. */
  case object PreviousInstruction extends Command
  /** Move to the first step. Answer: Command[Not]Executed. */
  case object First extends Command
  /** Move to the last step. Answer: Command[Not]Executed. */
  case object Last extends Command

  /** Get the current step. Answer: CurrentStep. */
  case object QueryStep extends Command
  case class CurrentStep(step: GuideStep, instruction: Instruction, steps: Seq[GuideStep]) extends Event

  /** Get 3D Layout as soon as ready. Answer: Knitted3DLayout. */
  case object GetKnitted3D extends Command
  case class Knitted3DLayout(knitted: Knitted3D, layout: Layout[Stitch3D]) extends Event

  /** Subscribe to ChangeEvent. Answer: Command[Not]Executed. */
  case object Subscribe extends Command
  /** Unsubscribe from ChangeEvent. Answer: Command[Not]Executed. */
  case object Unsubscribe extends Command
  /** Notification, receive when subscribed. */
  sealed trait Notification extends Event
  /** Notification sent whenever the current step/instruction changes. */
  case class ChangeEvent(newStep: GuideStep, newInstruction: Instruction) extends Notification

  private case object NotifyStepChange extends Command
  private case class SetPattern(pattern: NeedleActionRow, workingFrom: Needle, workingUntil: Needle) extends Event

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

  /** Manages the loaded plan and subscriptions; delegates commands to the GuiderForPlan. */
  private class Guider(machine: ActorRef) extends Actor {
    var current = Option.empty[ActorRef]
    var subscribers = Set.empty[ActorRef]

    context.actorOf(Machine.subscription(machine))

    override def receive = subscriptionHandling orElse {
      case cmd@LoadPlan(plan) =>
        current foreach (_ ! PoisonPill)
        val newCurrent = context actorOf GuiderForPlan.props(plan)
        newCurrent ! NotifyStepChange
        current = Some(newCurrent)
        sender ! CommandExecuted(cmd)

      case cmd: Command =>
        current
          .map(_ forward cmd)
          .getOrElse(sender ! CommandNotExecuted(cmd, "no plan is loaded"))

      case SetPattern(pattern, from, until) =>
        machine ! Machine.SetWorkingZone(from, until)
        machine ! Machine.LoadNeedlePattern(pattern)

      case cmd: Machine.Notification =>
        current.foreach(_ forward cmd)
    }

    def subscriptionHandling: Receive = {
      case cmd@Subscribe =>
        subscribers += sender
        context watch sender
        sender ! CommandExecuted(cmd)
      case cmd@Unsubscribe =>
        subscribers -= sender
        context unwatch sender
        sender ! CommandExecuted(cmd)
      case Terminated if subscribers.contains(sender) =>
        subscribers -= sender
      case notification: Notification =>
        subscribers foreach (_ ! notification)
    }
  }

  /** Keeps track of the position within a plan and answers all commands. */
  private class GuiderForPlan(plan: Plan) extends Actor {
    val layouter = context actorOf Layouter.props(plan.run.output3D)
    val steps = GuideParser(plan)
    var currentStep = steps.head
    var currentInstruction = currentStep.instructions.head

    private def modStep(offset: Int) = {
      val newPos = currentStep.position.shiftOption(offset)
      newPos foreach { pos =>
        currentStep = steps(pos.index)
        currentInstruction = currentStep.instructions.head
      }
      newPos.isDefined
    }
    private def modInstruction(offset: Int) = {
      require(Math.abs(offset) == 1, "Can only mod by 1 or -1")
      val newPos = currentInstruction.position.shiftOption(offset)
      newPos.map { pos =>
        currentInstruction = currentStep.instructions(pos.index)
        true
      }.getOrElse {
        if (modStep(offset)) {
          if (offset == -1) currentInstruction = currentStep.instructions.last
          true
        } else false
      }
    }
    private def nextPattern(step: GuideStep, instruction: Instruction): NeedleActionRow = instruction.step match {
      case knit: KnitRow => knit.pattern
      case _ =>
        if (instruction.position.isLast) {
          if (step.position.isLast) AllNeedlesToB
          else {
            val nextStep = steps(step.position.shift(1).index)
            nextPattern(nextStep, nextStep.instructions.head)
          }
        } else nextPattern(step, step.instructions(instruction.position.shift(1).index))
    }

    override def receive = {
      case Machine.NextRow =>
        //forward to next knitting step if we're not already at one
        while (!currentStep.isKnitting) modInstruction(1)
        modInstruction(1)
        self ! NotifyStepChange

      case GetKnitted3D =>
        implicit val timeout: Timeout = 10.minutes
        (layouter ? Layouter.Get).map {
          case layout: Layout[Stitch3D] => Knitted3DLayout(plan.run.output3D, layout)
        }.pipeTo(sender)

      case QueryStep =>
        sender ! CurrentStep(currentStep, currentInstruction, steps)

      case cmd@First =>
        currentStep = steps.head
        currentInstruction = currentStep.instructions.head
        self ! NotifyStepChange
        sender ! CommandExecuted(cmd)
      case cmd@Last =>
        currentStep = steps.last
        currentInstruction = currentStep.instructions.last
        self ! NotifyStepChange
        sender ! CommandExecuted(cmd)

      case cmd@NextStep =>
        if (modStep(1)) {
          self ! NotifyStepChange
          sender ! CommandExecuted(cmd)
        } else sender ! CommandNotExecuted(cmd, "already at last")
      case cmd@PreviousStep =>
        if (modStep(-1)) {
          self ! NotifyStepChange
          sender ! CommandExecuted(cmd)
        } else sender ! CommandNotExecuted(cmd, "already at first")

      case cmd@NextInstruction =>
        if (modInstruction(1)) {
          self ! NotifyStepChange
          sender ! CommandExecuted(cmd)
        } else sender ! CommandNotExecuted(cmd, "already at last")
      case cmd@PreviousInstruction =>
        if (modInstruction(-1)) {
          self ! NotifyStepChange
          sender ! CommandExecuted(cmd)
        } else sender ! CommandNotExecuted(cmd, "already at first")

      case NotifyStepChange =>
        val pattern = nextPattern(currentStep, currentInstruction)
        val workings = currentInstruction.before.workingNeedles
        val margin = KCarriage.width / 2 - 8
        val working = workings.headOption.map(first => (first, workings.last)).getOrElse((Needle.middle - 1, Needle.middle + 1))
        context.parent ! SetPattern(pattern, working._1 safeSubtract margin, working._2 safeAdd margin)
        context.parent ! ChangeEvent(currentStep, currentInstruction)
    }
  }
  private object GuiderForPlan {
    def props(plan: Plan) = Props(new GuiderForPlan((plan)))
  }
}