package controllers

import java.awt.Color
import java.util.UUID
import javax.inject._
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import play.api.mvc._
import play.api.libs.json._
import knit.Yarn
import knit.plan._
import models.Project._
import models.ProjectRepository._

class ProjectController @Inject()(@Named("project-repository") projectRepository: ActorRef)(implicit ec: ExecutionContext) extends Controller {
  implicit val timeout: Timeout = 10.seconds

  case class Error(title: String, code: String, detail: Option[String] = None, id: UUID = UUID.randomUUID())

  implicit val errorFormat = Json.format[Error]
  implicit val projectInfoFormat = Json.format[ProjectInfo]
  implicit val updateProjectInfoFormat = Json.format[UpdateProjectInfo]
  implicit object ColorWrite extends Writes[Color] {
    override def writes(color: Color) = {
      val value = color.getRGB | 0xff000000
      JsString("#" + value.toHexString.drop(2))
    }
  }
  implicit val yarnWrites = Json.writes[Yarn]
  implicit object StitchWrite extends Writes[Stitch] {
    override def writes(stitch: Stitch) = stitch match {
      case NoStitch => Json.obj("type" -> "no")
      case EmptyStitch => Json.obj("type" -> "empty")
      case PlainStitch(yarns) => Json.obj("type" -> "plain", "yarns" -> yarns.map(_.name))
      case PurlStitch(yarns) => Json.obj("type" -> "purl", "yarns" -> yarns.map(_.name))
      case CastOnStitch(yarns) => Json.obj("type" -> "castOn", "yarns" -> yarns.map(_.name))
      case CastOffStitch(yarns) => Json.obj("type" -> "castOff", "yarns" -> yarns.map(_.name))
    }
  }

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
        case ProjectInfoUpdateInvalid(detail) =>
          req.errorStatus(400, Error("Invalid data", "INVALID_DATA", Some(detail)))
      }
    )
  }

  def knitted(id: UUID) = ActionOnProject(id).async { req =>
    (req.project ? GetKnitted).map {
      case KnittedResponse(knitted) =>
        Ok(Json.obj(
          "mainBed" -> knitted.mainBed.data,
          "yarns" ->
            (knitted.mainBed.data.flatten.flatMap {
              case PlainStitch(yarns) => yarns
              case PurlStitch(yarns) => yarns
              case CastOnStitch(yarns) => yarns
              case CastOffStitch(yarns) => yarns
              case _ => Seq.empty[Yarn]
            }).toSet[Yarn]
        ))
      case NoProduct =>
        req.errorStatus(400, Error("Creation not finished (no PUT received)", "INVALID_STATUS"))
    }
  }
}