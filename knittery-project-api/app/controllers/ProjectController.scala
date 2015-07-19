package controllers

import java.util.UUID

import akka.actor.ActorRef
import akka.pattern.ask
import javax.inject._
import akka.util.Timeout
import model.ProjectRepository.{ProjectCreated, CreateProject}
import play.api.mvc._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ProjectController @Inject()(@Named("project-repository") projectRepository: ActorRef)(implicit ec: ExecutionContext) extends Controller {
  implicit val timeout: Timeout = 1.second

  def create() = Action.async {
    (projectRepository ? CreateProject).map {
      case ProjectCreated(id) =>
        val url = routes.ProjectController.get(id).url
        Created(s"Project $id created.").withHeaders(LOCATION -> url)
    }
  }

  def get(id: UUID) = Action {
    NotImplemented
  }
}
