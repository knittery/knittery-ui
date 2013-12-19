import play.api._
import play.api.libs.concurrent.Akka
import akka.io.IO
import rxtxio.Serial
import connector.BrotherConnector
import controllers.SerialSimulator

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    implicit val system = Akka.system(app)

    val port = "/dev/cu.example"
    val connector = BrotherConnector.props(port, SerialSimulator.manager)
  }

}