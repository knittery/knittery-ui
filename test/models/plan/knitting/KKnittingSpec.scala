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

  private trait plain extends Yarns {
    import KCarriage._
    val redPiece = YarnPiece(red)
    def plain = State(SinkerPlate(Some(redPiece)))
    def plainH = State(SinkerPlate(Some(redPiece)), Settings(holdingCamLever = HoldingCamH))

    def state(pos: Needle => NeedlePosition) = {
      val (yarn, ns) = Needle.all.foldLeft((redPiece: YarnFlow, Map.empty[Needle, NeedleState])) {
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

  "plain single bed knitting" should {
    "knit plain red stitches with one yarn and needles to B" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleB), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must_== Needle.all.map(_ => PlainStitch(red))
    }
    "knit plain red stitches with one yarn and needles to D" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleD), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must_== Needle.all.map(_ => PlainStitch(red))
    }
    "knit plain red stitches with one yarn and needles to E" in new plain {
      val knit = new KKnitting(plain, state(_ => NeedleE), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must_== Needle.all.map(_ => PlainStitch(red))
    }
  }
  "plain single bed knitting with holdingCamLever=H" should {
    "knit nothing with one yarn and needles to E" in new plain {
      val knit = new KKnitting(plainH, state(_ => NeedleE), ToRight)
      val end = knit(AllNeedlesToB).check()
      end.output.rows.size must_== 1
      end.output.rows(0) must_== Needle.all.map(_ => NoStitch)
    }
  }
}