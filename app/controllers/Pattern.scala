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

object Pattern extends Controller {

  protected def machine = Akka.system.actorSelection("akka://application/user/machine")
 
  case class GaugeFormData(widthInCm: BigDecimal, countStitches: Int, heightInCm: BigDecimal, countRows: Int)
  
  val uploadForm = Form(
         mapping(
               "widthInCm" -> bigDecimal,
               "countStitches" -> number,
               "heightInCm" -> bigDecimal,
               "countRows" -> number
         )(GaugeFormData.apply)(GaugeFormData.unapply)
   )
   
  def show = Action {
    val img = ImageIO.read(new File("public/images/pattern.png"))
    val pattern = NeedlePattern.loadCenter(img)
    machine ! Machine.LoadPattern(pattern)
    Ok(views.html.pattern())
  }

  def upload = Action(parse.multipartFormData) { implicit request =>
    val gaugeData = uploadForm.bindFromRequest().get
    
    request.body.file("pattern").map { pattern =>
      val filename = pattern.filename 
      val contentType = pattern.contentType

      val gauge = new Gauge(gaugeData.widthInCm.doubleValue, gaugeData.countStitches, gaugeData.heightInCm.doubleValue, gaugeData.countRows)
      
      pattern.ref.moveTo(new File("public/images/pattern.png"), true)
      Redirect(routes.Pattern.show()) 
    }.getOrElse {
      Redirect(routes.Pattern.show).flashing(
        "error" -> "Upload failed"
      )
    }
  }
}