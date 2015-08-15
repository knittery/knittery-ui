package knit.plan.knitting

import org.specs2.mutable.Specification
import org.specs2.matcher.{MustMatchers, Matchers}
import scalaz._
import Scalaz._
import knit._
import knit.plan.{NeedleState, Stitch}


trait KnittingStateMatchers extends Matchers with MustMatchers {
  implicit class ValidationCheck[A](val r: Validation[String, A]) {
    def check(): A = {
      ("Failure: " + r) <==> (r.isSuccess must beTrue)
      val Success(a) = r
      a
    }
  }

  def beAllStitch(stitch: Stitch) =
    contain(stitch).forall
  def beAtPosition(pos: NeedlePosition) = {
    def adapt(in: Needle => NeedleState) = in.positions.all
    contain(pos).forall ^^ adapt _
  }
  def carryYarn(yarn: YarnPiece*) = {
    def adapt(in: Needle => NeedleState) = in.all.map(_.yarn.map(_.start))
    contain(===(yarn.toSet)).forall ^^ adapt _
  }
  def carryYarnOn(filter: Needle => Boolean, yarn: YarnPiece*) = {
    def adapt(in: Needle => NeedleState) = Needle.all.filter(filter).map(in).map(_.yarn.map(_.start))
    contain(===(yarn.toSet)).forall ^^ adapt _
  }

  def beYarn(yarns: YarnPiece*) = {
    def adapt(in: Set[YarnFlow]) = in.toSeq.map(_.start)
    containAllOf(yarns) ^^ adapt _
  }
}
