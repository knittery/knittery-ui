package models.guide

import java.net.URI

case class WikiReference(id: String) {
  def apply(implicit wiki: WikiResolver) = wiki.resolve(id)
}

trait WikiResolver {
  def resolve(id: String): URI
}