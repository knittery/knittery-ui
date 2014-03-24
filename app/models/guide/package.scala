package models

import play.api.i18n.Lang

package object guide {
  trait Text {
    def apply(implicit lang: Lang): String
  }
}
