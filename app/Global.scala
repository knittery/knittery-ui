import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scalaz._
import Scalaz._
import play.api._
import play.api.libs.concurrent.Akka
import akka.actor._
import akka.io.IO
import rxtxio.Serial
import squants.space.LengthConversions._
import models._
import models.gauge.StandardGauge
import models.connector.{BrotherConnector, SerialPortMock}
import models.machine.Machine
import models.plan._
import models.planners._
import models.guide._
import utils._
import knittings.LaptopCase


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
    //    val plan = sockPlan
    //    val plan = handyPlan
    //    val plan = laptopPlan
    //    val plan = laptopPlanGradient
    //    val plan = laptopPlanCheckerboard
    val plan = laptopPlanDissolvingCheckerboard
    Logger.info("Initial plan loaded.")
    guider ! Guider.LoadPlan(plan.valueOr(e => throw new RuntimeException(e)))
  }

  @volatile private var _machine: Option[ActorRef] = None
  def machine = _machine.getOrElse(throw new IllegalStateException("not started"))

  @volatile private var _guider: Option[ActorRef] = None
  def guider = _guider.getOrElse(throw new IllegalStateException("not started"))

  private def laptopPlan = {
    val bg = Yarn("black", Color.black)
    val fg = Yarn("white", Color.white)

    //stitch width: 7
    //size of dell laptop
    Examples.laptopHuelle(23.1d, 33.7d, 2.1d, (29, 46), bg, fg, 7).plan()
  }

  private def laptopPlanGradient = {
    val top = Yarn("white", Color.white)
    val bottom = Yarn("blue", Color.cyan)
    implicit val gauge = StandardGauge(34, 42)

    val patterns = LaptopCase.gradient(top, bottom, bottom)
    LaptopCase.form(25.cm, 36.cm, 2.cm, 10.cm, 1.5.cm, patterns).plan()
  }

  private def laptopPlanCheckerboard = {
    val yarnA = Yarn("cyan", Color.cyan)
    val yarnB = Yarn("orange", Color.orange)
    implicit val gauge = StandardGauge(34, 42)

    val patterns = LaptopCase.checkerboardPattern(yarnA, yarnB, 2.cm)
    LaptopCase.form(25.cm, 36.cm, 2.cm, 10.cm, 1.5.cm, patterns).plan()
  }

  private def laptopPlanDissolvingCheckerboard = {
    val yarnA = Yarn("cyan", Color.cyan)
    val yarnB = Yarn("orange", Color.orange)
    implicit val gauge = StandardGauge(34, 42)

    val patterns = LaptopCase.dissolvingCheckerboardPattern(yarnA, yarnB, 2.cm)
    LaptopCase.form(25.cm, 36.cm, 2.cm, 10.cm, 1.5.cm, patterns).plan()
  }

  private def handyPlan = {
    val bg = Yarn("white", Color.white)
    val img = ImageIO.read(new File("pattern/mobile_sleeve_kid_koala.png"))
    Examples.handyHuelle(img, bg, KCarriage.TensionDial(8, 1)).plan()
  }

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