package controllers

import play.api.Play._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import akka.actor._
import akka.util.ByteString
import rxtxio.Serial._
import scala.concurrent.Future
import connector.SerialPortMock

/** Simulates the serial port the knitting machine is normally attached to. */
object SerialSimulator extends Controller {

  def show = Action {
    Ok(views.html.serialsimulator())
  }

  def send = Action { req =>
    req.body.asText match {
      case Some(text) =>
        val decoded = text.replaceAll("\\\\t", "\t") + "\n"
        channelToPort.push(ByteString(decoded, encoding))
        Ok(Json.obj("status" -> "ok"))
      case None =>
        Ok(Json.obj("status" -> "error", "reason" -> "no data"))
    }
  }

  val channelToPort = SerialPortMock.channel
  val enumeratorFromPort = SerialPortMock.enumerator
  def enumeratorToPort = SerialPortMock.enumeratorOfChannel

  def subscribe = WebSocket.using[String] { request =>
    val out = enumeratorFromPort &> Enumeratee.map(">" + _.decodeString(encoding))
    val in = enumeratorToPort &> Enumeratee.map("<" + _.decodeString(encoding))
    (Iteratee.ignore, out.interleave(in))
  }

  val encoding = "ASCII"
}