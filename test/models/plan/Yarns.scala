package models.plan

import org.specs2.specification.Scope
import java.awt.Color

trait Yarns extends Scope {
  val red = Yarn("red", Color.red)
  val green = Yarn("green", Color.green)
}
