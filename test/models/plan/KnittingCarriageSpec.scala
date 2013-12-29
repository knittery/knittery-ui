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
    def carriageKC2MC(pattern: NeedleActionRow) = KnittingCarriage(KCarriage, KCarriageSettings(KC2, mc = true), Some(red), Some(green), Some(pattern))

    def allA(n: Needle) = NeedleState(NeedleA, None)
    def allBWithRed(n: Needle) = NeedleState(NeedleB, Some(red))
    def allDWithRed(n: Needle) = NeedleState(NeedleD, Some(red))
    def allEWithRed(n: Needle) = NeedleState(NeedleE, Some(red))
    def allBDEvenOddWithRedGreen(n: Needle) = {
      if (n.index % 2 == 0) NeedleState(NeedleB, Some(red))
      else NeedleState(NeedleD, Some(green))
    }

    def allBPattern(n: Needle) = NeedleToB
    def allDPattern(n: Needle) = NeedleToD
    def evenOddPattern(n: Needle) = if (n.index % 2 == 0) NeedleToB else NeedleToD
  }
  implicit class RichResult(val r: Validation[String, (NeedleStateRow, KnittedRow)]) {
    def check = {
      r.isSuccess must beTrue
      val Success((needles, knitted)) = r
      (needles, knitted)
    }
  }

  "KKnittigCarriage on NL" should {
    "knit plain red stiches with one yarn and needles to B" in new K {
      val (needles, knitted) = carriageNL(Right)(allBWithRed).check
      knitted.stiches must_== (1 to Needle.count).map(_ => PlainStich(red))
    }
    "knit plain red stiches with one yarn and needles to D" in new K {
      val (needles, knitted) = carriageNL(Right)(allDWithRed).check
      knitted.stiches must_== (1 to Needle.count).map(_ => PlainStich(red))
    }
    "knit plain red stiches with one yarn and needles to E" in new K {
      val (needles, knitted) = carriageNL(Right)(allEWithRed).check
      knitted.stiches must_== (1 to Needle.count).map(_ => PlainStich(red))
    }

    "move all needles to B and put red yarn on them if they were at B" in new K {
      val (needles, knitted) = carriageNL(Right)(allBWithRed).check
      needles.all must contain(NeedleState(NeedleB, Some(red))).forall
    }
    "move all needles to B and put red yarn on them if they were at D" in new K {
      val (needles, knitted) = carriageNL(Right)(allBWithRed).check
      needles.all must contain(NeedleState(NeedleB, Some(red))).forall
    }
    "move all needles to B and put red yarn on them if they were at E" in new K {
      val (needles, knitted) = carriageNL(Right)(allBWithRed).check
      needles.all must contain(NeedleState(NeedleB, Some(red))).forall
    }
  }

  "KKnittigCarriage on KC2 with MC" should {
    "knit plain red stiches with one yarn and needles to B" in new K {
      val (needles, knitted) = carriageKC2MC(evenOddPattern)(Right)(allBWithRed).check
      knitted.stiches must_== (0 until Needle.count).map(_ => PlainStich(red))
    }
    "move every second needle to D in the even odd pattern" in new K {
      val (needles, knitted) = carriageKC2MC(evenOddPattern)(Right)(allBWithRed).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== (if (index % 2 == 0) NeedleB else NeedleD)
          yarn must_== Some(red)
      }
    }
    "move every needle to B and have red/green on the needles in the all B-pattern after even/odd" in new K {
      val (needles, knitted) = carriageKC2MC(allBPattern)(Right)(allBDEvenOddWithRedGreen).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== NeedleB
          yarn must_== (if (index % 2 == 0) Some(red) else Some(green))
      }
    }
    "move every needle to D and have red/green on the needles in the all D-pattern after even/odd" in new K {
      val (needles, knitted) = carriageKC2MC(allDPattern)(Right)(allBDEvenOddWithRedGreen).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== NeedleD
          yarn must_== (if (index % 2 == 0) Some(red) else Some(green))
      }
    }
    "knit plain red/green stich pattern with even odd needles and even odd pattern" in new K {
      val (needles, knitted) = carriageKC2MC(evenOddPattern)(Right)(allBDEvenOddWithRedGreen).check
      knitted.stiches.zipWithIndex.foreach {
        case (stich, index) if index % 2 == 0 => stich must_== PlainStich(red)
        case (stich, index) => stich must_== PlainStich(green)
      }
    }
  }
}