import java.io.File
import javax.imageio.ImageIO
import scala.concurrent.Await
import scala.concurrent.duration._
import play.api._
import play.api.libs.concurrent.Akka
import akka.actor._
import akka.util.Timeout
import akka.io.IO
import rxtxio.Serial
import connector._
import models._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    implicit val system = Akka.system(app)

    val port = app.configuration.getString("serial.port").getOrElse("SerialSimulator")
    def serialManager = {
      if (port == "SerialSimulator") system.actorOf(SerialPortMock.props, "SerialPortManager")
      else IO(Serial)
    }
    Logger.info(s"Using serial port $port. Specify using -Dserial.port=/dev/cu.myport")

    val connector = BrotherConnector.props(port, serialManager)
    _machine = Some(system.actorOf(Machine.props(connector), "machine"))

    val img = ImageIO.read(new File("example.png"))
    val pattern = NeedlePattern.loadCenter(img)
    machine ! Machine.LoadPattern(pattern)
  }

  @volatile private var _machine: Option[ActorRef] = None
  def machine = _machine.getOrElse(throw new IllegalStateException("Not started."))
}