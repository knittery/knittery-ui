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

    Logger.info("Loading initial plan...")
    //    val plan = imagePlan
    //    val plan = examplePlan
    //    val plan = tubePlan
    //    val plan = decreasingTubePlan
    val plan = sockPlan
    Logger.info("Initial plan loaded.")
    guider ! Guider.LoadPlan(plan.valueOr(e => throw new RuntimeException(e)))
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
    val firstYarn = Yarn("green", Color.green)
    val first = YarnPiece(firstYarn)
    val bg = YarnPiece(yarn1)
    val last = YarnPiece(Yarn("yellow", Color.yellow))

    val width = 20
    val height = 5
    val planner = Cast.onClosed(MainBed, Needle.atIndex(100 - width / 2), Needle.atIndex(100 + width / 2), firstYarn) >>
      Basics.knitRowWithK(yarnA = Some(first)) >>
      (0 to height / 2).toVector.traverse { _ =>
        Basics.knitRowWithK(yarnA = Some(bg))
      } >>
      FairIslePlanner.singleBed(checkerboard(Needle.count, height)) >>
      (0 to height / 2).toVector.traverse { _ =>
        Basics.knitRowWithK(yarnA = Some(bg))
      } >>
      Basics.knitRowWithK(yarnA = Some(bg)) >>
      Basics.knitRowWithK(yarnA = Some(last)) >>
      Cast.offClosed(MainBed, last)
    planner.plan()
  }

  private def tubePlan = Examples.tube(10, 40, YarnPiece(Yarn("red", Color.red))).plan()
  private def decreasingTubePlan = Examples.decreasingTube(20, 60, YarnPiece(Yarn("red", Color.red))).plan()
  private def sockPlan = Examples.sock(12, 20, 15, YarnPiece(Yarn("red", Color.red))).plan()

  private def imagePlan = {
    val img = ImageIO.read(new File("example.png"))
    val planner = Examples.imageRagDoubleBed(img)
    planner.plan()
  }

}