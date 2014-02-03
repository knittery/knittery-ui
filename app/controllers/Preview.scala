package controllers

import play.api.mvc._

object Preview extends Controller {

  def show = Action {
    Ok(views.html.preview("TODO"))
  }

}