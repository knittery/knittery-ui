package utils

import scala.concurrent.duration._
import scala.util.control.NoStackTrace
import akka.actor._

/**
 * Actor that manages a subscription (renews on crashes).
 * If the subscription fails to set up this actor will crash.
 */
trait SubscriptionActor extends Actor {
  val to: ActorRef
  /** Message used to subscribe. */
  def subscribe: Any
  /** Checks if the subscription has been set up successful. */
  def subscribed: PartialFunction[Any, Boolean]
  /** Message used to unsubscribe. */
  def unsubscribe: Any

  /** Default forwards message to parent, override to change. */
  def onMessage(msg: Any): Unit = context.parent ! msg

  /** Timeout for setting up the subscription. */
  def timeout = 200.millis

  override final def preStart() = {
    context watch to
    to ! subscribe
    context.setReceiveTimeout(timeout)
  }
  override final def postStop() = to ! unsubscribe
  override final def receive = handleCrash orElse {
    case msg if subscribed.isDefinedAt(msg) =>
      if (subscribed(msg)) context become handleMessages
      else throw SubscribeFailed(msg) //crash, so our parent can decide what to do
    case ReceiveTimeout => throw SubscribeFailed("Timeout")
  }
  private def handleCrash: Receive = {
    case Terminated(`to`) => to ! subscribe
  }
  private def handleMessages: Receive = handleCrash orElse {
    case msg => onMessage(msg)
  }
}

case class SubscribeFailed(cause: Any) extends RuntimeException(s"Subscription failed $cause") with NoStackTrace
