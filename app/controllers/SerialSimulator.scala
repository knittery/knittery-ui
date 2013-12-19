package controllers

import play.api.Play._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor._
import akka.util.ByteString
import rxtxio.Serial._

/** Simulates the serial port the knitting machine is normally attached to. */
object SerialSimulator extends Controller {

  def show = Action {
    Ok(views.html.serialsimulator())
  }

  private val (enumeratorFromPort, channelFromPort) = Concurrent.broadcast[ByteString]
  private val (enumeratorToPort, channelToPort) = Concurrent.broadcast[ByteString]

  val manager = Akka.system.actorOf(Props(new ManagerActor))

  class ManagerActor extends Actor {
    override def receive = {
      case ListPorts => sender ! Ports(Vector("simulator"))
      case Open(port, bauds) =>
        val operator = context.actorOf(Props(new OperatorActor(sender)))
        sender ! Opened(operator, port)
    }
  }
  class OperatorActor(commander: ActorRef) extends Actor {
    val iteratee = Iteratee.foreach[ByteString](data => self ! Received(data))
    override def preStart = {
      enumeratorToPort(iteratee)
    }
    override def postStop = {
      commander ! Closed
    }

    override def receive = {
      case Close =>
        if (sender != commander) sender ! Closed
        context stop self
      case Write(data, ack) =>
        channelFromPort.push(data)
        if (ack != NoAck) sender ! ack
      case Received(data) => commander ! Received
    }
  }

}