package connector

import scala.collection.JavaConversions._
import akka.actor.Actor
import gnu.io.CommPort
import gnu.io.{ SerialPort => GnuSerialPort }
import gnu.io.CommPortIdentifier
import akka.actor.Props
import scala.util.{ Try, Success, Failure }
import gnu.io.SerialPortEventListener
import gnu.io.SerialPortEvent
import java.io.InputStream
import akka.util.{ ByteString, ByteStringBuilder }
import akka.actor.ActorRef

class SerialPort(name: String, baudRate: Int) extends Actor {
  import SerialPort._

  var handlers = Vector.empty[ActorRef]

  override def preStart = context become notConnected
  override def preRestart(reason: Throwable, msg: Option[Any]) = context.unbecome()

  private case object DataAvailable

  override def receive = notConnected

  def notConnected: Receive = {
    case Connect =>
      (for {
        id <- Try(CommPortIdentifier.getPortIdentifier(name))
        p <- Try(id.open(context.self.toString, 2000))
        sp <- p match {
          case s: GnuSerialPort => Success(s)
          case _ => Failure(new IllegalArgumentException("Not a SerialPort"))
        }
        _ <- Try {
          import GnuSerialPort._
          sp.setSerialPortParams(baudRate, DATABITS_8, STOPBITS_1, PARITY_NONE)
        }
      } yield sp) match {
        case Success(port) =>
          port.notifyOnDataAvailable(true);
          val toNotify = self
          port.addEventListener(new SerialPortEventListener() {
            override def serialEvent(event: SerialPortEvent) {
              toNotify ! DataAvailable
            }
          })
          context become connected(port)
          sender ! Connected
        case Failure(error) =>
          sender ! ConnectFailed(error.toString)
      }

    case Register(handler) =>
      handlers = handlers :+ handler

    case Disconnect =>
      sender ! Disconnected

    case DataAvailable => ()
    
    case Write(_) => ()
  }

  def connected(port: GnuSerialPort): Receive = {
    case Connect => sender ! Connected

    case Disconnect =>
      port.close()
      context.unbecome()

    case Register(handler) =>
      handlers = handlers :+ handler

    case DataAvailable =>
      val data = read(port.getInputStream)
      handlers.foreach(_ ! Received(data))
      
    case Write(data) =>
      port.getOutputStream write data.toArray
  }

  private def read(from: InputStream): ByteString = {
    val bsb = new ByteStringBuilder
    def doRead() {
      val data = from.read()
      if (data != -1) {
        bsb += data.toByte
        doRead
      }
    }
    doRead()
    bsb.result
  }
}

object SerialPort {
  def apply(name: String, baudRate: Int) = Props(classOf[SerialPort], name, baudRate)

  /** Names of all serial ports on this computer. */
  def ports = {
    val ids = CommPortIdentifier.getPortIdentifiers.asInstanceOf[java.util.Enumeration[gnu.io.CommPortIdentifier]]
    ids.map(_.getName)
  }

  sealed trait Command
  sealed trait Event

  case object Connect extends Command
  case object Connected extends Event
  case class ConnectFailed(reason: String) extends Event

  case object Disconnect extends Command
  case object Disconnected extends Event

  case class Register(handler: ActorRef) extends Command
  case class Unregister(handler: ActorRef) extends Command

  case class Received(data: ByteString) extends Event

  case class Write(data: ByteString) extends Command
}