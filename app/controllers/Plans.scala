package controllers

import java.io.File
import java.awt.Color
import javax.imageio.ImageIO
import play.api.Play._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Akka
import models._
import ch.knittery.pattern._
import models.units.{NumericStitchesConverter, NumericRowsConverter}
import models.guide._
import models.planners._
import knittings._

object Plans extends Controller {

  protected def guider = Akka.system.actorSelection(Akka.system / "guider")

  case class GaugeFormData(widthInCmImg: BigDecimal, heightInCmImg: BigDecimal, widthInCm: BigDecimal, countStitches: Int, heightInCm: BigDecimal, countRows: Int)

  val uploadPatternForm = Form(
    mapping(
      "widthInCmImg" -> bigDecimal,
      "heightInCmImg" -> bigDecimal,
      "widthInCm" -> bigDecimal,
      "countStitches" -> number,
      "heightInCm" -> bigDecimal,
      "countRows" -> number)(GaugeFormData.apply)(GaugeFormData.unapply))

  case class SockFormData(width: Int, shaft: Int, foot: Int)
  val sockForm = Form(mapping(
    "width" -> number,
    "shaft" -> number,
    "foot" -> number)(SockFormData.apply)(SockFormData.unapply))

  case class TensionFormData(tension: Int, thirds: Int)
  val tensionForm = Form(mapping(
    "tension" -> number,
    "thirds" -> number)(TensionFormData.apply)(TensionFormData.unapply))

  def show = Action {
    Ok(views.html.plans())
  }

  def loadImagePlan = Action(parse.multipartFormData) { implicit request =>
    request.body.file("img").map { patternFile =>
      val image = ImageIO.read(patternFile.ref.file)
      val planner = Examples.imageRag(image)
      val plan = planner.plan().valueOr(e => throw new RuntimeException(e))
      guider ! Guider.LoadPlan(plan)
      Redirect(routes.Plans.show())
    }.getOrElse {
      Redirect(routes.Plans.show).flashing(
        "error" -> "Upload failed")
    }
  }

  def loadImageDoubleBedPlan = Action(parse.multipartFormData) { implicit request =>
    val form = tensionForm.bindFromRequest.get
    val tension = KCarriage.TensionDial.apply(form.tension, form.thirds)

    request.body.file("imgDoubleBed").map { patternFile =>
      val image = ImageIO.read(patternFile.ref.file)
      val planner = Examples.imageRagDoubleBed(image, tension)
      val plan = planner.plan().valueOr(e => throw new RuntimeException(e))
      guider ! Guider.LoadPlan(plan)
      Redirect(routes.Plans.show())
    }.getOrElse {
      Redirect(routes.Plans.show).flashing(
        "error" -> "Upload failed")
    }
  }

  def loadPatternPlan = Action(parse.multipartFormData) { implicit request =>
    val gaugeFormData = uploadPatternForm.bindFromRequest().get
    val gauge = new Gauge(gaugeFormData.widthInCm.doubleValue, gaugeFormData.countStitches, gaugeFormData.heightInCm.doubleValue, gaugeFormData.countRows)

    request.body.file("pattern").map { patternFile =>
      val image = {
        //Pattern-Image will be converted, if a gauge is provided
        if (gauge.getStitchesPerCm > 0) {
          PatternConverter.convertToPattern(ImageIO.read(patternFile.ref.file),
            gaugeFormData.widthInCmImg.doubleValue, gaugeFormData.heightInCmImg.doubleValue, gauge)
        } else {
          ImageIO.read(patternFile.ref.file)
        }
      }

      ImageIO.write(image, "png", new File("public/images/pattern.png"))

      val planner = Examples.imageRag(image)
      val plan = planner.plan().valueOr(e => throw new RuntimeException(e))
      guider ! Guider.LoadPlan(plan)

      Redirect(routes.Plans.show())
    }.getOrElse {
      Redirect(routes.Plans.show).flashing(
        "error" -> "Upload failed")
    }
  }

  def loadSock = Action { implicit request =>
    val form = sockForm.bindFromRequest.get
    val plan = Sock(form.width.stitches, form.shaft.rows, form.foot.rows, Yarn("red", Color.red))
      .plan().valueOr(e => throw new RuntimeException(s"Invalid Plan: $e"))
    guider ! Guider.LoadPlan(plan)
    Redirect(routes.Plans.show())
  }
}