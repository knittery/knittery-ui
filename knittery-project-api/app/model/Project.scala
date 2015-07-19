package model

import java.util.UUID

import akka.actor.{Props, Actor}

object Project {
  def apply(id: UUID) = Props(new Project(id))

  sealed trait Message
}
class Project private(id: UUID) extends Actor {
  override def receive = ???
}
