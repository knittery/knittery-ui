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

  case class Error(title: String, code: String, detail: Option[String] = None, id: UUID = UUID.randomUUID())

  implicit val errorFormat = Json.format[Error]
  implicit val projectInfoFormat = Json.format[ProjectInfo]
  implicit val updateProjectInfoFormat = Json.format[UpdateProjectInfo]

  class ProjectRequest[A](val projectId: UUID, val project: ActorRef, request: Request[A]) extends WrappedRequest[A](request) {
    def successStatus(detail: String): Result = successStatus(Some(detail))
    def successStatus(detail: Option[String] = None) = {
      val json = Json.obj("id" -> projectId)
      Ok(detail.fold(json)(d => json + ("detail" -> JsString(d))))
    }
    def errorStatus(code: Int, error: Error*): Result = {
      val json = Json.obj("errors" -> error)
      Status(code)(json)
    }
  }
  def ProjectAction(projectId: UUID) = new ActionRefiner[Request, ProjectRequest] {
    def refine[A](input: Request[A]) = (projectRepository ? GetProject(projectId)).map {
      case ProjectFound(`projectId`, project) =>
        Right(new ProjectRequest(projectId, project, input))
      case ProjectNotFound(`projectId`) =>
        val error = Error("Project does not exist.", "NotFound", Some(s"Project with id $projectId does not exist."))
        Left(NotFound(Json.obj("errors" -> error)))
    }
  }
  def ActionOnProject(id: UUID) = Action andThen ProjectAction(id)


  def create() = Action.async { implicit req =>
    (projectRepository ? CreateProject).map {
      case ProjectCreated(id) =>
        val url = routes.ProjectController.get(id).absoluteURL()
        val json = Json.obj("id" -> id.toString)
        Created(json).withHeaders(LOCATION -> url)
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
        Future.successful(
          req.errorStatus(
            BAD_REQUEST,
            Error("Validation error", "VALIDATION_ERROR", Some(JsError.toJson(errors).toString))
          )
        )
      },
      update => (req.project ? update).map {
        case ProjectInfoUpdated => req.successStatus("Project updated.")
      }
    )
  }
}