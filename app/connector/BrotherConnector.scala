package connector

import scala.util.Try
import scala.concurrent.duration._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor._
import akka.util.ByteString
import rxtxio._
import Serial._
import models._
import play.Logger

class BrotherConnector(port: String, serialManager: ActorRef, parser: String => Option[MachineEvent]) extends Actor with ActorLogging {
  private val (rawEnumerator, channel) = Concurrent.broadcast[ByteString]
  def lineEnumerator = {
    def takeLine = for {
      line <- Enumeratee.takeWhile[Char](_ != '\n') &>> Iteratee.getChunks
      _ <- Enumeratee.take(1) &>> Iteratee.ignore[Char]
    } yield line.mkString

    rawEnumerator &>
      Enumeratee.mapConcat(_.toSeq.map(_.toChar)) &>
      Enumeratee.grouped(takeLine)
  }
  def eventEnumerator = {
    lineEnumerator &> Enumeratee.mapFlatten(in => Enumerator(parser(in).toSeq: _*))
  }

  override def preStart = {
    serialManager ! Open(port, 115200)
    context.setReceiveTimeout(5.seconds)
  }

  override def receive = {
    case Opened(operator, `port`) =>
      log.info(s"Serial port connection to brother opened on $port")
      //Start sending parsed events to the listener
      eventEnumerator(Iteratee.foreach(event => context.parent ! event))
      context.setReceiveTimeout(Duration.Undefined)
      context become open(operator)

    case CommandFailed(Open(port, _), error) =>
      throw new RuntimeException("Could not open serial port for brother", error)

    case ReceiveTimeout =>
      throw new RuntimeException("Could not open serial port for brother within timeout")
  }

  def open(operator: ActorRef): Receive = {
    case Received(data) =>
      log.debug(s"Input for serial port ${data.decodeString("ASCII")}")
      //Feed into the parser that will then send it to the listener
      channel.push(data)

    case Closed =>
      throw new RuntimeException("Unexpected close of serial port")
  }
}

/**
 *  Actor that will send MachineEvents to a listener. The actor will crash if there
 *  is a problem with the connection to the machine.
 */
object BrotherConnector {
  def props(port: String, serialManager: ActorRef) = {
    Props(new BrotherConnector(port, serialManager, parser))
  }

  private val carriageWidth = 24
  def parser(input: String): Option[MachineEvent] = {
    input.split('\t').toList match {
      case needle :: position :: direction :: carriage :: cpos :: _ if needle.startsWith("@") =>
        val value = for {
          index <- Try(needle.drop(1).toInt)
          index2 <- Try(position.toInt)
          n <- Try(Needle.atIndex(index))
          p <- Try(cpos match {
            case "<" => CarriageLeft((carriageWidth + index2).max(0).min(24))
            case ">" => CarriageRight((24 - (index2 - 199)).max(0).min(24))
            case "_" => CarriageOverNeedles(n)
            case other => throw new IllegalArgumentException(s"Unknown needle position: $other")
          })
          d <- Try(direction match {
            case "<-" => Left
            case "->" => Right
            case other => throw new IllegalArgumentException(s"Unknown direction: $other")
          })
          c <- Try(carriage match {
            case "K" => Some(KCarriage)
            case "L" => Some(LCarriage)
            case "G" => Some(GCarriage)
            case " " => None
          })
        } yield PositionUpdate(p, d, c)
        value.
          map(Some(_)).
          recover {
            case e =>
              Logger.debug(s"Failed to parse input from machine: ${e.getMessage}")
              None
          }.
          get

      case malformed if malformed.nonEmpty =>
        Logger.debug(s"Malformed input from machine: $malformed")
        None
      case empty => None
    }
  }
}
