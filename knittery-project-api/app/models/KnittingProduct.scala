package models

import java.awt.Color

import play.api.libs.json._
import squants.space.LengthConversions._
import scalaz._
import Scalaz._
import scalaz.Validation.FlatMap._
import knit._
import knit.gauge.StandardGauge
import knit.units.Gauge
import knit.plan.{Optimizers, Plan}


/** Product that can be knitted. */
case class KnittingProduct(kind: String, plan: Plan, json: JsValue)
object KnittingProduct {

  /** Parse a knitting product from JSON. */
  def fromJson(json: JsObject) = {
    json.validate[ProductChoice].map(_.get)
      .fold(JsError.toJson(_).toString.failure, _.success)
      .flatMap(_.build)
  }

  /** Allows to implement case classes that consist only of Option-values where exactly one must be defined. */
  private trait Choice[A] extends Product {
    require(productIterator.map({ case o: Option[A] => o }).filter(_.isDefined).size == 1)
    def get: A = productIterator.map({ case o: Option[A] => o }).find(_.isDefined).get.get
  }

  private case class ProductChoice(laptopCase: Option[LaptopTemplate]) extends Choice[KnittingProductBuilder]

  private sealed trait KnittingProductBuilder {
    def build: Validation[String, KnittingProduct]
  }


  /// Implementations

  private case class LaptopTemplate(width: Double, height: Double, thickness: Double,
    frontGap: Option[Double], lashLength: Option[Double], pattern: LaptopTemplate.PatternChoice) extends KnittingProductBuilder {
    val kind = "laptopCase"
    implicit val gauge = StandardGauge(34, 42, 5.tension)
    def build = {
      val patterns = pattern.get.patterns
      products.laptop.Form.apply(
        width = width.cm,
        height = height.cm,
        thickness = thickness.cm,
        topGap = frontGap.getOrElse(2d).cm,
        lash = lashLength.getOrElse(10d).cm,
        patterns = patterns
      ).plan(Optimizers.no).map { plan =>
        val json = Json.obj(kind -> Json.toJson(this)(LaptopTemplate.format))
        KnittingProduct(kind, plan, json)
      }
    }
  }
  private object LaptopTemplate {
    import products.laptop._
    private def yarnForHex(hex: String) =
      Yarn(hex, Color.decode(hex))

    sealed trait Pattern {
      def patterns(implicit gauge: Gauge): Dimensions => Patterns
    }

    case class Checkerboard(size: Double, color1: String, color2: String,
      dissolveExponent: Option[Double], seed: Option[Long]) extends Pattern {
      def patterns(implicit gauge: Gauge) = {
        val yarnA = yarnForHex(color1)
        val yarnB = yarnForHex(color2)
        dissolveExponent.fold {
          products.laptop.Checkerboard(yarnA, yarnB, size.cm)
        } { exp =>
          products.laptop.Checkerboard.dissolving(yarnA, yarnB, size.cm, exp, seed.getOrElse(0))
        }
      }
    }

    case class Gradient(topColor: String, bottomColor: String, lashColor: String, seed: Option[Long]) extends Pattern {
      def patterns(implicit gauge: Gauge) = {
        products.laptop
          .Gradient(yarnForHex(topColor), yarnForHex(bottomColor), yarnForHex(lashColor), seed.getOrElse(0))
      }
    }

    case class PatternChoice(checkerboard: Option[Checkerboard] = None, gradient: Option[Gradient] = None) extends Choice[Pattern]

    implicit val checkerboardFormat = Json.format[Checkerboard]
    implicit val gradientFormat = Json.format[Gradient]
    implicit val patternChoiceFormat = Json.format[PatternChoice]
    implicit val format = Json.format[LaptopTemplate]
  }


  // JSON

  import LaptopTemplate.format
  private implicit val format = Json.format[ProductChoice]
}