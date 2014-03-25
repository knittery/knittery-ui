package models

import play.api.i18n.Lang

package object guide {
  trait Text extends (Lang => String) {
    def apply(lang: Lang): String
    def value(implicit lang: Lang) = apply(lang)
  }
}
