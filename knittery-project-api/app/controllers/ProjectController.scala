package controllers

import java.util.UUID

import akka.actor.ActorRef
import akka.pattern.ask
import javax.inject._
import akka.util.Timeout
import model.Project.{ProjectInfo, GetInfo}
import model.ProjectRepository._
import play.api.mvc._
import play.api.libs.json._
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._

class ProjectController @Inject()(@Named("project-repository") projectRepository: ActorRef)(implicit ec: ExecutionContext) extends Controller {
  implicit val timeout: Timeout = 1.second

  implicit val projectInfoFormat = Json.format[ProjectInfo]

  def create() = Action.async {
    (projectRepository ? CreateProject).map {
      case ProjectCreated(id) =>
        val url = routes.ProjectController.get(id).url
        Created(s"Project $id created.").withHeaders(LOCATION -> url)
    }
  }

  def get(id: UUID) = Action.async {
    (projectRepository ? GetProject(id)).flatMap {
      case ProjectFound(`id`, project) =>
        (project ? GetInfo).map {
          case info: ProjectInfo => Ok(Json.toJson(info))
        }
      case ProjectNotFound(`id`) =>
        Future.successful(NotFound(s"Project $id does not exist."))
    }
  }
}