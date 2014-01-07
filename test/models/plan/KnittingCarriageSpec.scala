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
    def carriageKC2plain(pattern: NeedleActionRow) = KnittingCarriage(KCarriage, KCarriageSettings(KC2), Some(red), Some(green), Some(pattern))
    def carriageKC2MC(pattern: NeedleActionRow) = KnittingCarriage(KCarriage, KCarriageSettings(KC2, mc = true), Some(red), Some(green), Some(pattern))

    def allA(n: Needle) = NeedleState(NeedleA)
    def allBWithRed(n: Needle) = NeedleState(NeedleB, red)
    def allDWithRed(n: Needle) = NeedleState(NeedleD, red)
    def allEWithRed(n: Needle) = NeedleState(NeedleE, red)
    def allBDEvenOddWithRedGreen(n: Needle) = {
      if (n.index % 2 == 0) NeedleState(NeedleB, red)
      else NeedleState(NeedleD, green)
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

  "K-KnittigCarriage on NL" should {
    "knit plain red stitches with one yarn and needles to B" in new K {
      val (needles, knitted) = carriageNL(Right)(allBWithRed).check
      knitted.stitches must_== (1 to Needle.count).map(_ => PlainStitch(red))
    }
    "knit plain red stitches with one yarn and needles to D" in new K {
      val (needles, knitted) = carriageNL(Right)(allDWithRed).check
      knitted.stitches must_== (1 to Needle.count).map(_ => PlainStitch(red))
    }
    "knit plain red stitches with one yarn and needles to E" in new K {
      val (needles, knitted) = carriageNL(Right)(allEWithRed).check
      knitted.stitches must_== (1 to Needle.count).map(_ => PlainStitch(red))
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

  "K-KnittigCarriage on KC2 with plain" should {
    "knit plain red stitches with one yarn and needles to B" in new K {
      val (needles, knitted) = carriageKC2plain(evenOddPattern)(Right)(allBWithRed).check
      knitted.stitches must_== (0 until Needle.count).map(_ => PlainStitch(red))
    }
    "move every second needle to D in the even odd pattern" in new K {
      val (needles, knitted) = carriageKC2plain(evenOddPattern)(Right)(allBWithRed).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== (if (index % 2 == 0) NeedleB else NeedleD)
          yarn must_== List(red)
      }
    }
    "move every needle to B and have red on the needles in the all B-pattern after even/odd" in new K {
      val (needles, knitted) = carriageKC2plain(allBPattern)(Right)(allBDEvenOddWithRedGreen).check
      needles.all must contain(NeedleState(NeedleB, red)).forall
    }
    "move every needle to D and have red/green on the needles in the all D-pattern after even/odd" in new K {
      val (needles, knitted) = carriageKC2plain(allDPattern)(Right)(allBDEvenOddWithRedGreen).check
      needles.all must contain(NeedleState(NeedleD, red)).forall
    }
    "knit plain red/green stitch pattern with even odd needles and even odd pattern" in new K {
      val (needles, knitted) = carriageKC2plain(evenOddPattern)(Right)(allBDEvenOddWithRedGreen).check
      knitted.stitches.zipWithIndex.foreach {
        case (stitch, index) if index % 2 == 0 => stitch must_== PlainStitch(red)
        case (stitch, index) => stitch must_== PlainStitch(green)
      }
    }
  }

  "K-KnittigCarriage on KC2 with MC" should {
    "knit plain red stitches with one yarn and needles to B" in new K {
      val (needles, knitted) = carriageKC2MC(evenOddPattern)(Right)(allBWithRed).check
      knitted.stitches must_== (0 until Needle.count).map(_ => PlainStitch(red))
    }
    "move every second needle to D in the even odd pattern" in new K {
      val (needles, knitted) = carriageKC2MC(evenOddPattern)(Right)(allBWithRed).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== (if (index % 2 == 0) NeedleB else NeedleD)
          yarn must_== List(red)
      }
    }
    "move every needle to B and have red/green on the needles in the all B-pattern after even/odd" in new K {
      val (needles, knitted) = carriageKC2MC(allBPattern)(Right)(allBDEvenOddWithRedGreen).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== NeedleB
          yarn must_== (if (index % 2 == 0) List(red) else List(green))
      }
    }
    "move every needle to D and have red/green on the needles in the all D-pattern after even/odd" in new K {
      val (needles, knitted) = carriageKC2MC(allDPattern)(Right)(allBDEvenOddWithRedGreen).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== NeedleD
          yarn must_== (if (index % 2 == 0) List(red) else List(green))
      }
    }
    "knit plain red/green stitch pattern with even odd needles and even odd pattern" in new K {
      val (needles, knitted) = carriageKC2MC(evenOddPattern)(Right)(allBDEvenOddWithRedGreen).check
      knitted.stitches.zipWithIndex.foreach {
        case (stitch, index) if index % 2 == 0 => stitch must_== PlainStitch(red)
        case (stitch, index) => stitch must_== PlainStitch(green)
      }
    }
  }
}