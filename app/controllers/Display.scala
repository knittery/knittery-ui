package controllers

import play.api.mvc._
import akka.actor._
import models._
import models.Machine._

object Display extends Controller {

  def show = Action {
    Ok(views.html.display())
  }
  
  def subscribe = TODO

}