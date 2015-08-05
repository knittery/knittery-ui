package controllers

import play.api.libs.json.Json
import play.api.mvc._

class Application extends Controller {

  def index = Action {
    Redirect(routes.Application.swaggerUI)
  }

  def api = Action {
    Ok(Json.obj("api-versions" -> List("v1")))
  }

  def swaggerUI = Action {
    Redirect("/swagger-ui/index.html?url=/api/swagger.json")
  }

}
