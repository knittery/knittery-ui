package connector

import scala.util.Try
import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor._
import akka.util.ByteString
import rxtxio._
import Serial._
import models._
import play.Logger

class BrotherConnector(port: String, parser: String => Option[MachineEvent]) extends Actor with ActorLogging {
  protected def serialManager: ActorRef = akka.io.IO(Serial)(context.system)

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

  override def preStart = {
    serialManager ! Open(port, 115200)
  }

  override def receive = {
    case Opened(operator, `port`) =>
      log.info(s"Serial port connection to brother opened on $port")
      context become open(operator)
    case CommandFailed(Open(port, _), error) =>
      throw new RuntimeException("Could not open serial port for brother", error)
  }

  def open(operator: ActorRef): Receive = {
    case Received(data) => channel.push(data)
    case Closed => throw new RuntimeException("Unexpected close of serial port")
  }
}

object BrotherConnector {
  def props(port: String) = Props(new BrotherConnector(port, parser))

  def parser(input: String): Option[MachineEvent] = {
    input.split('\t').toList match {
      case needle :: direction :: carriage :: cpos :: _ if needle.startsWith("@") =>
        val value = for {
          index <- Try(needle.drop(1).toInt)
          n <- Try(Needle.atIndex(index))
          p <- Try(cpos match {
            case "<" => CarriageLeft(0) //  TODO change protocol to include that
            case ">" => CarriageRight(0) // TODO change protocol to include that
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
