package controllers

import scala.concurrent.duration._
import play.api.Play._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import models._
import models.machine.Machine._
import utils.JsonSerialization._
import java.io.File
import javax.imageio.ImageIO
import models.machine.Machine
import ch.knittery.pattern._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import java.awt.image.BufferedImage

object Pattern extends Controller {

  protected def machine = Akka.system.actorSelection(Akka.system / "machine")

  case class GaugeFormData(widthInCmImg: BigDecimal, heightInCmImg: BigDecimal, widthInCm: BigDecimal, countStitches: Int, heightInCm: BigDecimal, countRows: Int)

  val uploadForm = Form(
    mapping(
      "widthInCmImg" -> bigDecimal,
      "heightInCmImg" -> bigDecimal,
      "widthInCm" -> bigDecimal,
      "countStitches" -> number,
      "heightInCm" -> bigDecimal,
      "countRows" -> number)(GaugeFormData.apply)(GaugeFormData.unapply))

  def show = Action {
    Ok(views.html.pattern())
  }

  def upload = Action(parse.multipartFormData) { implicit request =>
    val gaugeFormData = uploadForm.bindFromRequest().get
    val gauge = new Gauge(gaugeFormData.widthInCm.doubleValue, gaugeFormData.countStitches, gaugeFormData.heightInCm.doubleValue, gaugeFormData.countRows)

    request.body.file("pattern").map { patternFile =>
      val image = {
        //Pattern-Image will be converted, if a gauge is provided
        if (gauge.getStitchesPerCm() > 0) {
          PatternConverter.convertToPattern(ImageIO.read(patternFile.ref.file),
            gaugeFormData.widthInCmImg.doubleValue, gaugeFormData.heightInCmImg.doubleValue, gauge)
        } else {
          ImageIO.read(patternFile.ref.file)
        }
      }

      ImageIO.write(image, "png", new File("public/images/pattern.png"))

      // TODO send the pattern to the guide..
      //      val pattern = NeedlePattern.loadCenter(image)
      //      machine ! Machine.LoadPattern(pattern)

      Redirect(routes.Pattern.show())
    }.getOrElse {
      Redirect(routes.Pattern.show).flashing(
        "error" -> "Upload failed")
    }
  }
}