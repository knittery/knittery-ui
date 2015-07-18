package utils

import scala.concurrent.ExecutionContext
import akka.actor._
import play.api.libs.iteratee._
import Concurrent._

object ActorEnumerator {

  /**
   * Returns a linked pair of actor and enumerator.
   * All messages sent to the actor will be returned by the enumerator.
   */
  def apply(children: Props*)(implicit system: ActorRefFactory, e: ExecutionContext) = {
    case class Init(channel: Channel[Any])
    case object StopNormal
    case class Link(to: ActorRef)
    case class Start(props: Props)
    val actor = system.actorOf(Props(new Actor {
      var channel: Option[Channel[Any]] = None
      var stopMsgPending = true
      override def preRestart(cause: Throwable, msg: Option[Any]) {
        channel foreach (_ end cause)
      }
      override def postStop() = {
        if (stopMsgPending) channel foreach (_.end())
      }
      override def receive = {
        case Init(c) => channel = Some(c)
        case StopNormal =>
          stopMsgPending = false
          context stop self
        case Link(to) => context watch to
        case Start(props) => context.actorOf(props)
        case msg => channel foreach (_ push msg)
      }
    }))
    val enumerator = Concurrent.unicast[Any](actor ! Init(_),
      actor ! StopNormal,
      (_, _) => actor ! StopNormal)
    children foreach (actor ! Start(_))
    (enumerator, actor)
  }

  /**
   * Returns a linked pair of actor and enumerator.
   * All messages sent to the actor will be returned by the enumerator.
   */
  def enumerator(children: Props*)(implicit system: ActorRefFactory, e: ExecutionContext) =
    apply(children: _*)._1
}