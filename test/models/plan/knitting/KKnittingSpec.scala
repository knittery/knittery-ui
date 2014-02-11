package models.plan.knitting

import scalaz._
import Scalaz._
import org.specs2.mutable.Specification
import models._
import models.plan._

class KKnittingSpec extends Specification {
  implicit class ValidationCheck[A](val r: Validation[String, A]) {
    def check(): A = {
      ("Failure: " + r) <==> (r.isSuccess must beTrue)
      val Success(a) = r
      a
    }
  }

  private trait StateSupport {
    def stateWithYarn(pos: Needle => NeedlePosition, yarnToUse: YarnFlow) = {
      val (yarn, ns) = Needle.all.foldLeft((yarnToUse, Map.empty[Needle, NeedleState])) {
        case ((yarn, r), n) =>
          val p = pos(n)
          if (p.isWorking) {
            val yarn2 = yarn.next(1)
            (yarn2, r + (n -> NeedleState(p, yarn2)))
          } else {
            (yarn, r + (n -> NeedleState(p)))
          }
      }
      KnittingState.initial.modifyNeedles(ns).
        attachYarn(YarnAttachment(yarn, Needle.all.reverse.head))
    }

  }
  private trait plain extends Yarns with StateSupport {
    import KCarriage._
    val redPiece = YarnPiece(red)
    def plain = State(SinkerPlate(Some(redPiece)))
    def plainH = State(SinkerPlate(Some(redPiece)), Settings(holdingCamLever = HoldingCamH))
    def state(pos: Needle => NeedlePosition) = stateWithYarn(pos, redPiece)
  }
  private trait part extends Yarns with StateSupport {
    import KCarriage._
    val redPiece = YarnPiece(red)
    def part = State(SinkerPlate(Some(redPiece)), Settings(partLeft = true, partRight = true))
    def partH = State(SinkerPlate(Some(redPiece)), Settings(partLeft = true, partRight = true, holdingCamLever = HoldingCamH))
    def state(pos: Needle => NeedlePosition) = stateWithYarn(pos, redPiece)
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

  "plain single bed knitting" should {
    "knit nothing with all needles to A" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleA), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(EmptyStitch)
      end.needles must beAtPosition(NeedleA)
      end.needles must carryYarn()
    }
    "knit plain red stitches with needles to B" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleB), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(PlainStitch(red))
      end.needles must beAtPosition(NeedleB)
      end.needles must carryYarn(redPiece)
    }
    "knit plain red stitches with needles to D" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleD), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(PlainStitch(red))
      end.needles must beAtPosition(NeedleB)
      end.needles must carryYarn(redPiece)
    }
    "knit plain red stitches with needles to E" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleE), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(PlainStitch(red))
      end.needles must beAtPosition(NeedleB)
      end.needles must carryYarn(redPiece)
    }
  }
  "plain single bed knitting with holdingCamLever=H" should {
    "knit nothing with all needles to A" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleA), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(EmptyStitch)
      end.needles must beAtPosition(NeedleA)
      end.needles must carryYarn()
    }
    "knit plain red stitches with needles to B" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleB), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(PlainStitch(red))
      end.needles must beAtPosition(NeedleB)
      end.needles must carryYarn(redPiece)
    }
    "knit plain red stitches with needles to D" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleD), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(PlainStitch(red))
      end.needles must beAtPosition(NeedleB)
      end.needles must carryYarn(redPiece)
    }
    "knit nothing with needles to E" in new plain {
      val knit = new KKnitting(plainH, state(_ => NeedleE), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(NoStitch)
      end.needles must beAtPosition(NeedleE)
      end.needles must carryYarn(redPiece)
    }
  }
  "plain single bed knitting with D-pattern" should {
    "knit nothing with all needles to A" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleA), ToRight)
      val end = knit(AllNeedlesToD).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(EmptyStitch)
      end.needles must beAtPosition(NeedleA)
      end.needles must carryYarn()
    }
    "knit plain red stitches with needles to B and move needles to D" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleB), ToRight)
      val end = knit(AllNeedlesToD).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(PlainStitch(red))
      end.needles must beAtPosition(NeedleD)
      end.needles must carryYarn(redPiece)
    }
  }
  "part single bed knitting" should {
    "knit nothing with all needles to A" in new part {
      val knit = new KKnitting(part, state(_ => NeedleA), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(EmptyStitch)
      end.needles must beAtPosition(NeedleA)
      end.needles must carryYarn()
    }
    "knit nothing with all needles to B" in new part {
      val knit = new KKnitting(part, state(_ => NeedleB), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(NoStitch)
      end.needles must beAtPosition(NeedleB)
      end.needles must carryYarn(redPiece)
    }
    "knit plain red stitches with needles to D" in new part {
      val knit = new KKnitting(part, state(_ => NeedleD), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(PlainStitch(red))
      end.needles must beAtPosition(NeedleB)
      end.needles must carryYarn(redPiece)
    }
    "knit plain red stitches with needles to E" in new part {
      val knit = new KKnitting(part, state(_ => NeedleE), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must beAllStitch(PlainStitch(red))
      end.needles must beAtPosition(NeedleB)
      end.needles must carryYarn(redPiece)
    }
  }
}