import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.concurrent.Await
import scala.concurrent.duration._
import scalaz._
import Scalaz._
import play.api._
import play.api.libs.concurrent.Akka
import akka.actor._
import akka.util.Timeout
import akka.io.IO
import rxtxio.Serial
import models._
import models.connector.{ BrotherConnector, SerialPortMock }
import models.machine.Machine
import models.plan._
import models.planners._
import models.guide._
import utils._

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
    _guider = Some(system.actorOf(Guider.props(machine), "guider"))

    //    guider ! Guider.LoadPlan(imagePlan)
    guider ! Guider.LoadPlan(examplePlan)
  }

  @volatile private var _machine: Option[ActorRef] = None
  def machine = _machine.getOrElse(throw new IllegalStateException("not started"))

  @volatile private var _guider: Option[ActorRef] = None
  def guider = _guider.getOrElse(throw new IllegalStateException("not started"))

  private def examplePlan = {
    val yarn1 = Yarn("red", Color.red)
    val yarn2 = Yarn("blue", Color.blue)
    def checkerboard(w: Int, h: Int): Matrix[Yarn] = {
      val s = Stream.from(0).map(i => if (i % 2 == 0) yarn1 else yarn2)
      (0 until h).map { i =>
        val c = if (i % 2 == 0) s else s.drop(1)
        c.take(w).toIndexedSeq
      }
    }
    val width = 20
    val height = 10
    val bg = YarnPiece(yarn1)
    val planner = Cast.onClosed(Needle.atIndex(100 - width / 2), Needle.atIndex(100 + width / 2), yarn1) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(bg)) >>
      //      FairIslePlanner.singleBed(checkerboard(Needle.count, height)) >>
      (0 to height).toVector.traverse { _ =>
        Basics.knitRowWithK(KCarriage.Settings(), Some(bg))
      } >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(bg)) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(bg))
    Cast.offClosed(bg)
    planner.plan().valueOr(e => throw new RuntimeException(e))
  }

  private val imagePlan = {
    val img = ImageIO.read(new File("example.png"))
    val planner = Examples.imageRagDoubleBed(img)
    planner.plan().valueOr(e => throw new RuntimeException(e))
  }

}