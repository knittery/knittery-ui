import play.api._
import play.api.libs.concurrent.Akka
import akka.actor._
import akka.io.IO
import rxtxio.Serial
import connector.BrotherConnector
import controllers.SerialSimulator
import models._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    implicit val system = Akka.system(app)

    val port = "/dev/cu.example"
    val connector = BrotherConnector.props(port, SerialSimulator.manager)
    _machine = Some(system.actorOf(Machine.props(connector)))
  }

  @volatile private var _machine: Option[ActorRef] = None
  def machine = _machine.getOrElse(throw new IllegalStateException("Not started."))

}