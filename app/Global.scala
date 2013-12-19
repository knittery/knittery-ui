import scala.concurrent.Await
import scala.concurrent.duration._
import play.api._
import play.api.libs.concurrent.Akka
import akka.actor._
import akka.util.Timeout
import rxtxio.Serial
import connector.BrotherConnector
import controllers.SerialSimulator
import models._

object Global extends GlobalSettings {

  private def serialManager(implicit system: ActorSystem) = {
    implicit val timeout = 1.second
    val selection = SerialSimulator.manager
    val f = selection.resolveOne(timeout)
    Await.result(f, timeout)
  }

  override def onStart(app: Application) {
    implicit val system = Akka.system(app)
    val port = "/dev/cu.example"
    val connector = BrotherConnector.props(port, serialManager)
    _machine = Some(system.actorOf(Machine.props(connector), "machine"))
  }

  @volatile private var _machine: Option[ActorRef] = None
  def machine = _machine.getOrElse(throw new IllegalStateException("Not started."))
}