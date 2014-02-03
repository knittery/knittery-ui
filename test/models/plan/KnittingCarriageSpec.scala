package models.plan

import scalaz._
import Scalaz._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import models._
import java.awt.Color

class KnittingCarriageSpec extends Specification {
  private trait K extends Yarns {
    val redFlow = YarnPiece(red)
    val greenFlow = YarnPiece(green)
    def carriageK = carriageKPlain(AllNeedlesToB)
    def carriageKPlain(pattern: NeedleActionRow) = {
      val state = KCarriage.State(KCarriage.SinkerPlate(Some(redFlow), None))
      KnittingCarriage(state, Map.empty, pattern)
    }
    def carriageKMC(pattern: NeedleActionRow) = {
      val state = KCarriage.State(KCarriage.SinkerPlate(Some(redFlow), Some(greenFlow)),
        KCarriage.Settings(mc = true))
      KnittingCarriage(state, Map.empty, pattern)
    }

    def allA(n: Needle) = NeedleState(NeedleA)
    def allBWithRed(n: Needle) = NeedleState(NeedleB, redFlow)
    def allDWithRed(n: Needle) = NeedleState(NeedleD, redFlow)
    def allEWithRed(n: Needle) = NeedleState(NeedleE, redFlow)
    def allBDEvenOddWithRedGreen(n: Needle) = {
      if (n.index % 2 == 0) NeedleState(NeedleB, redFlow)
      else NeedleState(NeedleD, greenFlow)
    }

    def allBPattern(n: Needle) = NeedleToB
    def allDPattern(n: Needle) = NeedleToD
    def evenOddPattern(n: Needle) = if (n.index % 2 == 0) NeedleToB else NeedleToD
  }
  implicit class RichResult[A](val r: Validation[String, A]) {
    def check = {
      r.isSuccess must beTrue
      val Success(a) = r
      a
    }
  }
  implicit class RichNSR(val nsr: NeedleStateRow) {
    def ignoreFlow = {
      nsr.andThen(v => v.copy(yarn = v.yarn.map(_.start)))
    }
  }

  "K-KnittigCarriage without pattern" should {
    "knit plain red stitches with one yarn and needles to B" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageK(ToRight, allBWithRed).check
      knitted.all must_== (1 to Needle.count).map(_ => PlainStitch(red))
    }
    "knit plain red stitches with one yarn and needles to D" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageK(ToRight, allDWithRed).check
      knitted.all must_== (1 to Needle.count).map(_ => PlainStitch(red))
    }
    "knit plain red stitches with one yarn and needles to E" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageK(ToRight, allEWithRed).check
      knitted.all must_== (1 to Needle.count).map(_ => PlainStitch(red))
    }
    "move all needles to B and put red yarn on them if they were at B" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageK(ToRight, allBWithRed).check
      needles.ignoreFlow.all must contain(NeedleState(NeedleB, Some(redFlow))).forall
    }
    "move all needles to B and put red yarn on them if they were at D" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageK(ToRight, allBWithRed).check
      needles.ignoreFlow.all must contain(NeedleState(NeedleB, Some(redFlow))).forall
    }
    "move all needles to B and put red yarn on them if they were at E" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageK(ToRight, allBWithRed).check
      needles.ignoreFlow.all must contain(NeedleState(NeedleB, Some(redFlow))).forall
    }
  }
  "K-KnittigCarriage on KC2 with plain" should {
    "knit plain red stitches with one yarn and needles to B" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKPlain(evenOddPattern)(ToRight, allBWithRed).check
      knitted.all must_== (0 until Needle.count).map(_ => PlainStitch(red))
    }
    "move every second needle to D in the even odd pattern" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKPlain(evenOddPattern)(ToRight, allBWithRed).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== (if (index % 2 == 0) NeedleB else NeedleD)
          yarn.map(_.start) must_== Set(redFlow)
      }
    }
    "move every needle to B and have red on the needles in the all B-pattern after even/odd" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKPlain(allBPattern)(ToRight, allBDEvenOddWithRedGreen).check
      needles.ignoreFlow.all must contain(NeedleState(NeedleB, redFlow)).forall
    }
    "move every needle to D and have red/green on the needles in the all D-pattern after even/odd" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKPlain(allDPattern)(ToRight, allBDEvenOddWithRedGreen).check
      needles.ignoreFlow.all must contain(NeedleState(NeedleD, redFlow)).forall
    }
    "knit plain red/green stitch pattern with even odd needles and even odd pattern" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKPlain(evenOddPattern)(ToRight, allBDEvenOddWithRedGreen).check
      knitted.all.zipWithIndex.foreach {
        case (stitch, index) if index % 2 == 0 => stitch must_== PlainStitch(red)
        case (stitch, index) => stitch must_== PlainStitch(green)
      }
    }
  }

  "K-KnittigCarriage on KC2 with MC" should {
    "knit plain red stitches with one yarn and needles to B" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKMC(evenOddPattern)(ToRight, allBWithRed).check
      knitted.all must_== (0 until Needle.count).map(_ => PlainStitch(red))
    }
    "move every second needle to D in the even odd pattern" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKMC(evenOddPattern)(ToRight, allBWithRed).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== (if (index % 2 == 0) NeedleB else NeedleD)
          yarn.map(_.yarn) must_== Set(red)
      }
    }
    "move every needle to B and have red/green on the needles in the all B-pattern after even/odd" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKMC(allBPattern)(ToRight, allBDEvenOddWithRedGreen).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== NeedleB
          yarn.map(_.yarn) must_== (if (index % 2 == 0) Set(red) else Set(green))
      }
    }
    "move every needle to D and have red/green on the needles in the all D-pattern after even/odd" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKMC(allDPattern)(ToRight, allBDEvenOddWithRedGreen).check
      needles.all.zipWithIndex.forall {
        case (NeedleState(pos, yarn), index) =>
          pos must_== NeedleD
          yarn.map(_.yarn) must_== (if (index % 2 == 0) Set(red) else Set(green))
      }
    }
    "knit plain red/green stitch pattern with even odd needles and even odd pattern" in new K {
      val KnittingCarriageResult(needles, _, _, knitted) = carriageKMC(evenOddPattern)(ToRight, allBDEvenOddWithRedGreen).check
      knitted.all.zipWithIndex.foreach {
        case (stitch, index) if index % 2 == 0 => stitch must_== PlainStitch(red)
        case (stitch, index) => stitch must_== PlainStitch(green)
      }
    }
  }
}