import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scalaz._
import Scalaz._
import play.api._
import play.api.libs.concurrent.Akka
import akka.actor._
import akka.io.IO
import rxtxio.Serial
import gnu.io.CommPortIdentifier
import squants.space.LengthConversions._
import models._
import models.gauge.{MeasuredGauge, StandardGauge}
import models.connector.{BrotherConnector, SerialPortMock}
import models.machine.Machine
import models.plan._
import models.planners._
import models.guide._
import models.units._
import utils._
import knittings._


object Global extends GlobalSettings {

  override def onStart(app: Application) {
    implicit val system = Akka.system(app)

    val port = app.configuration.getString("serial.port").getOrElse {
      val ports = {
        try {
          CommPortIdentifier.getPortIdentifiers.toList.map(_.asInstanceOf[CommPortIdentifier])
        }
        catch {
          case e: Throwable =>
            Logger.warn("cannot list serial ports", e)
            Nil
        }
      }
      val likelyPorts = ports.
        filter(_.getPortType == CommPortIdentifier.PORT_SERIAL).
        filterNot(_.getName.toLowerCase.contains("bluetooth"))
      println(s"ports = ${ports.map(_.getName)}")
      println(s"probable ports = ${likelyPorts.map(_.getName)}")
      likelyPorts.map(_.getName).
        headOption.getOrElse("SerialSimulator")
    }
    def serialManager = {
      if (port == "SerialSimulator") system.actorOf(SerialPortMock.props, "SerialPortManager")
      else IO(Serial)
    }
    Logger.info(s"Using serial port $port. Specify using -Dserial.port=/dev/cu.myport")

    val connector = BrotherConnector.props(port, serialManager)
    _machine = Some(system.actorOf(Machine.props(connector), "machine"))
    _guider = Some(system.actorOf(Guider.props(machine), "guider"))

    Logger.info("Loading initial plan...")
    //    val planner = imagePlan
    //    val planner = examplePlan
    //    val planner = tubePlan
    //    val planner = decreasingTubePlan
    //    val planner = sockPlan
    //    val planner = handyPlan
    //    val planner = laptopPlan
    //    val planner = laptopPlanGradient
    //    val planner = laptopPlanCheckerboard
    //    val planner = laptopPlanDissolvingCheckerboard
    val planner = rigaScarfPlan
    //    val planner = scarfGauge

    def optimizer(plan: Plan) = {
      val t0 = System.currentTimeMillis
      val result = Optimizers.all(plan)
      val time = (System.currentTimeMillis - t0) millis
      val percentage = (1 - result.steps.size.toDouble / plan.steps.size) * 100
      Logger.info(s"plan optimized in $time (${plan.steps.size} steps reduced by ${percentage.round.toInt}%)")
      result
    }
    val plan = planner.plan(optimizer)
    guider ! Guider.LoadPlan(plan.valueOr(e => throw new RuntimeException(e)))
    Logger.info("initial plan loaded.")
  }

  @volatile private var _machine: Option[ActorRef] = None
  def machine = _machine.getOrElse(throw new IllegalStateException("not started"))

  @volatile private var _guider: Option[ActorRef] = None
  def guider = _guider.getOrElse(throw new IllegalStateException("not started"))

  private def rigaScarfPlan = {
    val yarnA = Yarn("black", Color.black)
    val yarnB = Yarn("gray", Color.gray)
    implicit val gauge = MeasuredGauge(48 stitches, 14.5 cm, 96 rows, 27.6 cm, 6 tension)

    val img = ImageIO.read(new File("pattern/riga-pattern.png"))
    val tile = Helper.monochromeToPattern(img, yarnA, yarnB).transpose
    Scarf.rectangular(1.35 meters, 33 cm, dims => {
      tile.tile(dims.width.approx, dims.height.approx)
    })
  }

  private def scarfGauge = {
    val yarnA = Yarn("blue", Color.blue)
    val yarnB = Yarn("red", Color.red)

    implicit val gauge = StandardGauge(10, 10, 6 tension)
    val img = ImageIO.read(new File("pattern/riga-pattern.png"))
    val tile = Helper.monochromeToPattern(img, yarnB, yarnA).transpose
    Scarf.rectangular(100 cm, 48 cm, dims => {
      tile.tile(dims.width.approx, dims.height.approx)
    })
  }

  private def laptopPlan = {
    val bg = Yarn("black", Color.black)
    val fg = Yarn("white", Color.white)

    //stitch width: 7
    //size of dell laptop
    Examples.laptopHuelle(23.1d, 33.7d, 2.1d, (29, 46), bg, fg, 7)
  }

  private def laptopPlanGradient = {
    val top = Yarn("white", Color.white)
    val bottom = Yarn("blue", Color.cyan)
    implicit val gauge = StandardGauge(34, 42, 5.tension)

    val patterns = LaptopCase.gradient(top, bottom, bottom)
    LaptopCase.form(25.cm, 36.cm, 2.cm, 10.cm, 1.5.cm, patterns)
  }

  private def laptopPlanCheckerboard = {
    val yarnA = Yarn("cyan", Color.cyan)
    val yarnB = Yarn("orange", Color.orange)
    implicit val gauge = StandardGauge(34, 42, 5.tension)

    val patterns = LaptopCase.checkerboardPattern(yarnA, yarnB, 2.cm)
    LaptopCase.form(25.cm, 36.cm, 2.cm, 10.cm, 1.5.cm, patterns)
  }

  private def laptopPlanDissolvingCheckerboard = {
    val yarnA = Yarn("cyan", Color.cyan)
    val yarnB = Yarn("orange", Color.orange)
    implicit val gauge = StandardGauge(34, 42, 5.tension)

    val patterns = LaptopCase.dissolvingCheckerboardPattern(yarnA, yarnB, 2.cm)
    LaptopCase.form(25.cm, 36.cm, 2.cm, 10.cm, 1.5.cm, patterns)
  }

  private def handyPlan = {
    val bg = Yarn("white", Color.white)
    val img = ImageIO.read(new File("pattern/mobile_sleeve_kid_koala.png"))
    Examples.handyHuelle(img, bg, 8.3 tension)
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
    planner
  }

  private def tubePlan = Examples.tube(10, 40, YarnPiece(Yarn("red", Color.red)))
  private def decreasingTubePlan = Examples.decreasingTube(20, 60, YarnPiece(Yarn("red", Color.red)))

  private def sockPlan = {
    implicit val gauge = StandardGauge(36, 44, 7.tension)
    Sock.europeanSize(37, Yarn("red", Color.red))
  }

  private def littleSockPlan = {
    Sock(12.stitches, 20.rows, 15.rows, Yarn("red", Color.red))
  }

  private def imagePlan = {
    val img = ImageIO.read(new File("pattern/example.png"))
    Examples.imageRagDoubleBed(img, 7 tension)
  }

}