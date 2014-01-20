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
    _guider = Some(system.actorOf(Guider.props, "guider"))

    val img = ImageIO.read(new File("example.png"))
    val pattern = NeedlePattern.loadCenter(img)
    machine ! Machine.LoadPattern(pattern)

    guider ! Guider.LoadPlan(imagePlan)
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
    val width = 40
    val height = 20
    val planner = Cast.onClosed(Needle.atIndex(100 - width / 2), Needle.atIndex(100 + width / 2), yarn1) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      FairIslePlanner.singleBed(checkerboard(Needle.count, height), yarn1) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      Cast.offClosed(yarn1)
    planner.plan().valueOr(e => throw new RuntimeException(e))
  }

  private val imagePlan = {
    val img = ImageIO.read(new File("example.png"))
    val w = img.getWidth.min(200)
    val rgbs = (0 until img.getHeight).map { y =>
      (0 until w).map { x =>
        new Color(img.getRGB(x, y))
      }
    }
    val yarns = rgbs.flatten.toSet.zipWithIndex.map {
      case (color, i) => (color -> Yarn(s"Yarn $i", color))
    }.toMap
    val pattern = rgbs.matrixMap(yarns).reverseBoth

    val yarn1 = pattern(0)(0)
    val zero = 100 - w / 2
    val planner = Cast.onClosed(Needle.atIndex(zero), Needle.atIndex(zero + w-1), yarn1) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      FairIslePlanner.singleBed(pattern, yarn1) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      Cast.offClosed(yarn1)
    planner.plan().valueOr(e => throw new RuntimeException(e))
  }

}