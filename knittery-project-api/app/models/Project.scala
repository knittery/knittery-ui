package models

import java.util.UUID
import akka.actor.{Props, Actor}
import play.api.libs.json.{JsValue, JsObject}
import models.Project._


object Project {
  def apply(id: UUID) = Props(new Project(id))

  sealed trait Message
  case object GetInfo extends Message
  case class ProjectInfo(id: UUID, name: Option[String], kind: Option[String], product: Option[JsValue])

  case class UpdateProjectInfo(name: Option[String], product: JsObject) extends Message
  case object ProjectInfoUpdated extends Message
  case class ProjectInfoUpdateInvalid(detail: String) extends Message
}
class Project private(id: UUID) extends Actor {
  private var name = Option.empty[String]
  private var product = Option.empty[KnittingProduct]

  override def receive = {
    case GetInfo =>
      sender ! ProjectInfo(id, name, product.map(_.kind), product.map(_.json))

    case UpdateProjectInfo(name, product) =>
      KnittingProduct.fromJson(product)
        .fold(
          error => {
            sender ! ProjectInfoUpdateInvalid(error)
          },
          product => {
            this.product = Some(product)
            this.name = name
            sender ! ProjectInfoUpdated
          }
        )
  }
}
