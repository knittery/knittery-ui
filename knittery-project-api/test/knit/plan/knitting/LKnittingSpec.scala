package knit.plan.knitting

import org.specs2.mutable.Specification
import knit._
import LCarriage._

import scalaz.{Success, Validation}

class LKnittingSpec extends Specification with KnittingStateMatchers {
  private trait States extends StateSupport with Yarns {
    val yarn = YarnPiece(red)
    def lace = State(Settings(Lace))
    def stateBefore(pos: Needle => NeedlePosition = _ => NeedleB) = stateWithYarn(pos, yarn).moveCarriage(LCarriage, Right)
    def stateBeforeWithAtD(onD: Needle*) = stateBefore(n => if (onD.contains(n)) NeedleD else NeedleB)
    def n1 = Needle.middle
    def n2 = Needle.middle + 1
    def n3 = Needle.middle + 2
    def n4 = Needle.middle + 3
    def otherNeedles(n: Needle) = !(n == n1 || n == n2 || n == n3 || n == n4)
  }

  "lace pattern knitting" should {
    "leave alone none working needles" in new States {
      val before = stateBefore(_ => NeedleA)
      val knit = new LKnitting(lace, before, ToLeft)
      val after = knit(AllNeedlesToB).check()
      after.output must_== before.output
      after.needles(MainBed) must beAtPosition(NeedleA)
      after.needles(MainBed) must carryYarn()
    }

    "leave alone needles at B if knitting to left" in new States {
      val before = stateBefore()
      val knit = new LKnitting(lace, before, ToLeft)
      val after = knit(AllNeedlesToB).check()
      after.output must_== before.output
      after.needles(MainBed) must beAtPosition(NeedleB)
      after.needles(MainBed) must carryYarn(yarn)
    }

    "leave alone needles at B if knitting to right" in new States {
      val before = stateBefore().moveCarriage(LCarriage, Left)
      val knit = new LKnitting(lace, before, ToRight)
      val after = knit(AllNeedlesToB).check()
      after.output must_== before.output
      after.needles(MainBed) must beAtPosition(NeedleB)
      after.needles(MainBed) must carryYarn(yarn)
    }

    "move yarn on a D needle one needle to the left if knitting to left" in new States {
      val before = stateBeforeWithAtD(n2)
      val knit = new LKnitting(lace, before, ToLeft)
      val after = knit(AllNeedlesToB).check()
      after.output must_== before.output
      after.needles(MainBed) must beAtPosition(NeedleB)
      val needles = after.needles(MainBed).toMap
      needles(n1).yarn must beYarn(yarn, yarn)
      needles(n2).yarn must beYarn()
      needles(n3).yarn must beYarn(yarn)
      needles(n4).yarn must beYarn(yarn)
      after.needles(MainBed) must carryYarnOn(otherNeedles, yarn)
    }

    "move yarn on a D needle one needle to the right if knitting to right" in new States {
      val before = stateBeforeWithAtD(n2).moveCarriage(LCarriage, Left)
      val knit = new LKnitting(lace, before, ToRight)
      val after = knit(AllNeedlesToB).check()
      after.output must_== before.output
      after.needles(MainBed) must beAtPosition(NeedleB)
      val needles = after.needles(MainBed).toMap
      needles(n1).yarn must beYarn(yarn)
      needles(n2).yarn must beYarn()
      needles(n3).yarn must beYarn(yarn, yarn)
      needles(n4).yarn must beYarn(yarn)
      after.needles(MainBed) must carryYarnOn(otherNeedles, yarn)
    }

    "support multiple needles in D position" in new States {
      val before = stateBeforeWithAtD(n2, n4)
      val knit = new LKnitting(lace, before, ToLeft)
      val after = knit(AllNeedlesToB).check()
      after.output must_== before.output
      after.needles(MainBed) must beAtPosition(NeedleB)
      val needles = after.needles(MainBed).toMap
      needles(n1).yarn must beYarn(yarn, yarn)
      needles(n2).yarn must beYarn()
      needles(n3).yarn must beYarn(yarn, yarn)
      needles(n4).yarn must beYarn()
      after.needles(MainBed) must carryYarnOn(otherNeedles, yarn)
    }

    "not support two adjacent needles in D position" in new States {
      val before = stateBeforeWithAtD(n2, n3)
      val knit = new LKnitting(lace, before, ToLeft)
      knit(AllNeedlesToB).isFailure must beTrue
    }

    "must move B needles to the positions in the pattern" in new States {
      val before = stateBeforeWithAtD(n2, n4)
      val knit = new LKnitting(lace, before, ToLeft)
      val after = knit(n => if (n.index % 2 == 0) NeedleToB else NeedleToD).check()
      val m = after.needles(MainBed).toMap
      m.filter(_._1.index % 2 == 0).map(_._2.position) must contain[NeedlePosition](NeedleB).foreach
      m.filter(_._1.index % 2 != 0).map(_._2.position) must contain[NeedlePosition](NeedleD).foreach
    }

    "must move D needles to the positions in the pattern" in new States {
      val before = stateBeforeWithAtD(n1, n3)
      val knit = new LKnitting(lace, before, ToLeft)
      val after = knit(n => if (n.index % 2 == 0) NeedleToB else NeedleToD).check()
      val m = after.needles(MainBed).toMap
      m.filter(_._1.index % 2 == 0).map(_._2.position) must contain[NeedlePosition](NeedleB).foreach
      m.filter(_._1.index % 2 != 0).map(_._2.position) must contain[NeedlePosition](NeedleD).foreach
    }
  }

  // TODO fine lace pattern

}
