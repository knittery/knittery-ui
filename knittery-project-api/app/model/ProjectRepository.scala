package model

import java.util.UUID

import akka.actor.{ActorRef, Actor}
import model.ProjectRepository._

object ProjectRepository {
  sealed trait Message

  case object CreateProject extends Message
  case class ProjectCreated(id: UUID) extends Message

  case class GetProject(id: UUID) extends Message
  case class ProjectFound(id: UUID, actor: ActorRef) extends Message
  case class ProjectNotFound(id: UUID) extends Message
}

class ProjectRepository extends Actor {
  context become handler(Map.empty)

  def receive = PartialFunction.empty

  private def handler(projects: Map[UUID, ActorRef]): Receive = {
    case CreateProject =>
      val id = UUID.randomUUID()
      val actor = context actorOf Project(id)
      sender ! ProjectCreated(id)
      context become handler(projects + (id -> actor))

    case GetProject(id) =>
      sender() ! projects.get(id).fold[Message](ProjectNotFound(id))(ProjectFound(id, _))
  }
}
