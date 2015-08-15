package models

import java.util.UUID

import akka.actor.{Props, Actor}
import models.Project.{ProjectInfoUpdated, UpdateProjectInfo, ProjectInfo, GetInfo}

object Project {
  def apply(id: UUID) = Props(new Project(id))

  sealed trait Message
  case object GetInfo extends Message
  case class ProjectInfo(id: UUID, name: Option[String])

  case class UpdateProjectInfo(name: Option[String]) extends Message
  case object ProjectInfoUpdated extends Message
}
class Project private(id: UUID) extends Actor {
  private var name = Option.empty[String]

  override def receive = {
    case GetInfo =>
      sender ! ProjectInfo(id, name)

    case UpdateProjectInfo(name) =>
      this.name = name
      sender ! ProjectInfoUpdated
  }
}
