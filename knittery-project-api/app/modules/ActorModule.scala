package modules

import com.google.inject.AbstractModule
import model.ProjectRepository
import play.api.libs.concurrent.AkkaGuiceSupport

class ActorModule extends AbstractModule with AkkaGuiceSupport {
  def configure = {
    bindActor[ProjectRepository]("project-repository")
  }
}