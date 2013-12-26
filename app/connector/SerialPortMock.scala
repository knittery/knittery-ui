package connector

import scala.concurrent.Future
import akka.actor._
import akka.util.ByteString
import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits._
import rxtxio.Serial
import Serial._

/** Mock for a serial port. Uses the rxtxio.Serial interface. */
object SerialPortMock {
  /** Data that is sent to this port. */
  def enumerator = enumeratorFromPort
  /** Use to send data as the serial port. */
  def channel = channelToPort

  def enumeratorOfChannel = enumeratorToPort

  def props = Props(new ManagerActor)

  private val (enumeratorFromPort, channelFromPort) = Concurrent.broadcast[ByteString]
  private val (enumeratorToPort, channelToPort) = Concurrent.broadcast[ByteString]

  class ManagerActor extends Actor {
    override def receive = {
      case ListPorts => sender ! Ports(Vector("SerialPortMock"))
      case Open(port, bauds) =>
        val commander = sender
        val operator = context.actorOf(Props(new OperatorActor(commander)))
        sender ! Opened(operator, port)
    }
  }

  class OperatorActor(commander: ActorRef) extends Actor {
    @volatile var stopped = false
    val iteratee = Iteratee.fold2[ByteString, Unit](()) { (_, data) =>
      self ! data
      Future.successful((), stopped)
    }
    val encoding = "ASCII"

    override def preStart = {
      context watch commander
      enumeratorToPort(iteratee)
    }
    override def postStop = {
      stopped = true
      commander ! Closed
    }

    override def receive = {
      case Close =>
        if (sender != commander) sender ! Closed
        context stop self
      case Write(data, ack) =>
        channelFromPort.push(data)
        if (ack != NoAck) sender ! ack
        //Confirm stuff
        data.decodeString(encoding).split('\t').toList match {
          case "$" :: ">" :: pattern :: rest =>
            channel push ByteString(s"$$\t<\t$pattern", encoding)
          case _ => ()
        }
      case data: ByteString =>
        commander ! Received(data)
    }
  }

}