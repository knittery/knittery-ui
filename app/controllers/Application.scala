package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def javascriptRoutes = Action { implicit request ⇒
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        SerialSimulator.send, SerialSimulator.subscribe, //
        Display.subscribe,
        Preview.json,
        Guide.subscribe, Guide.next, Guide.previous, Guide.first, Guide.last,
        Guide.nextInstruction, Guide.previousInstruction, Guide.jumpTo)). //
      as("text/javascript")
  }
}