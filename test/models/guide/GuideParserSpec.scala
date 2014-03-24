package models.guide

import org.specs2.mutable.Specification
import models._
import models.plan._

class GuideParserSpec extends Specification {

  trait YarnPieces extends Yarns {
    val allNeedles = (n: Needle) => true
    val redPiece = YarnPiece(red)
    val greenPiece = YarnPiece(green)
  }

  private def plan(steps: Step*) =
    Plan(steps).valueOr(e => throw new RuntimeException(s"Invalid test-plan: $e"))

  trait oneLinePlan extends YarnPieces {
    val oneLinePlan = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      ClosedCastOff(MainBed, redPiece, allNeedles))
  }

  "GuideParser(oneLinePlan)" should {
    trait steps extends oneLinePlan {
      val steps = GuideParser(oneLinePlan)
    }
    "have 6 steps" in new steps {
      steps.size must_== 5
    }
    "have 1st step must be cast on" in new steps {
      val step = steps(0)
      step.title must_== "Cast on main bed"
      step.description must_== "Perform a closed cast on on the main bed with red on needles 1 until 40"
    }
  }

}
