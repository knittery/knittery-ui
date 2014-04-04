package models.guide

import java.net.URI

case class WikiReference(id: String) {
  def apply()(implicit resolver: WikiResolver) = resolver.resolve(id)
}

trait WikiResolver {
  def resolve(id: String): URI
}