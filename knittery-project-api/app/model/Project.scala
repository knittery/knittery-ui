package model

import java.util.UUID

import akka.actor.{Props, Actor}
import model.Project.{ProjectInfo, GetInfo}

object Project {
  def apply(id: UUID) = Props(new Project(id))

  sealed trait Message
  case object GetInfo extends Message
  case class ProjectInfo(id: UUID)
}
class Project private(id: UUID) extends Actor {
  override def receive = {
    case GetInfo =>
      sender ! ProjectInfo(id)
  }
}
