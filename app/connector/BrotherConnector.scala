package connector

import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor._
import akka.util.ByteString
import rxtxio._
import Serial._

class BrotherConnector(port: String) extends Actor with ActorLogging {
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