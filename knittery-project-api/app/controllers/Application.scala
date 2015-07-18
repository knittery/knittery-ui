package controllers

import play.api._
import play.api.mvc._

class Application extends Controller {

  def index = Action {
    Redirect(routes.Application.swaggerUI)
  }

  def swaggerUI = Action {
    Redirect("/swagger-ui/index.html?url=/v1/swagger.json#!/Project")
  }

}
