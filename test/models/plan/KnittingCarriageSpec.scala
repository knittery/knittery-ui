package models.plan

import scalaz._
import Scalaz._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import models._
import java.awt.Color

class KnittingCarriageSpec extends Specification {
  trait Yarns extends Scope {
    val red = Yarn("red", Color.red)
    val green = Yarn("green", Color.green)
  }
  trait K extends Yarns {
    val carriageNL = KnittingCarriage(KCarriage, KCarriageSettings(NL), Some(red), None, None)
    def allA(n: Needle) = NeedleState(NeedleA, None)
    def allBWithRed(n: Needle) = NeedleState(NeedleB, Some(red))
    def allDWithRed(n: Needle) = NeedleState(NeedleD, Some(red))
    def allEWithRed(n: Needle) = NeedleState(NeedleE, Some(red))
  }
  implicit class RichResult(val r: Validation[String, (NeedleStateRow, KnittedRow)]) {
    def check = {
      r.isSuccess must beTrue
      val Success((needles, knitted)) = r
      (needles, knitted)
    }
  }

  "KKnittigCarriage on NL" should {
    "knit plain stiches with one yarn and needles to B" in new K {
      val (needles, knitted) = carriageNL(Right)(allBWithRed).check
      knitted.stiches must_== (1 to Needle.count).map(_ => PlainStich(red))
    }
    "knit plain stiches with one yarn and needles to D" in new K {
      val (needles, knitted) = carriageNL(Right)(allDWithRed).check
      knitted.stiches must_== (1 to Needle.count).map(_ => PlainStich(red))
    }
    "knit plain stiches with one yarn and needles to E" in new K {
      val (needles, knitted) = carriageNL(Right)(allEWithRed).check
      knitted.stiches must_== (1 to Needle.count).map(_ => PlainStich(red))
    }
  }
}