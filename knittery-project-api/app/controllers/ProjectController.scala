package controllers

import java.util.UUID

import akka.actor.ActorRef
import akka.pattern.ask
import javax.inject._
import akka.util.Timeout
import model.Project.{ProjectInfoUpdated, UpdateProjectInfo, ProjectInfo, GetInfo}
import model.ProjectRepository._
import play.api.mvc._
import play.api.libs.json._
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._

class ProjectController @Inject()(@Named("project-repository") projectRepository: ActorRef)(implicit ec: ExecutionContext) extends Controller {
  implicit val timeout: Timeout = 1.second
  implicit val projectInfoFormat = Json.format[ProjectInfo]
  implicit val updateProjectInfoFormat = Json.format[UpdateProjectInfo]

  class ProjectRequest[A](val projectId: UUID, val project: ActorRef, request: Request[A]) extends WrappedRequest[A](request)
  def ProjectAction(projectId: UUID) = new ActionRefiner[Request, ProjectRequest] {
    def refine[A](input: Request[A]) = (projectRepository ? GetProject(projectId)).map {
      case ProjectFound(`projectId`, project) => Right(new ProjectRequest(projectId, project, input))
      case ProjectNotFound(`projectId`) => Left(NotFound(s"Project $projectId does not exist."))
    }
  }
  def ActionOnProject(id: UUID) = Action andThen ProjectAction(id)

  def create() = Action.async {
    (projectRepository ? CreateProject).map {
      case ProjectCreated(id) =>
        val url = routes.ProjectController.get(id).url
        Created(s"Project $id created.").withHeaders(LOCATION -> url)
    }
  }

  def get(id: UUID) = ActionOnProject(id).async { req =>
    (req.project ? GetInfo).map {
      case info: ProjectInfo => Ok(Json.toJson(info))
    }
  }

  def update(id: UUID) = ActionOnProject(id).async(BodyParsers.parse.json) { req =>
    req.body.validate[UpdateProjectInfo].fold(
      errors => {
        Future.successful(BadRequest(Json.obj("message" -> JsError.toJson(errors))))
      },
      update => (req.project ? update).map {
        case ProjectInfoUpdated => Ok("Project updated")
      }
    )
  }
}