package models.guide

import org.specs2.mutable.Specification
import play.api.test.WithApplication
import play.api.i18n.Lang
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
  trait tenLinePlan extends YarnPieces {
    val tenLinePlan = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, allNeedles))
  }

  trait twoYarnPlan extends YarnPieces {
    val twoYarnPlan = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      ThreadYarnK(Some(greenPiece), None),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, greenPiece, allNeedles))
  }

  implicit val lang = Lang("en")
  def beText(value: String) = (be_==(value)) ^^ ((t: Text) => t(lang))

  "GuideParser(oneLinePlan)" should {
    trait steps extends WithApplication with oneLinePlan {
      val steps = GuideParser(oneLinePlan)
    }
    "have 6 steps" in new steps {
      steps.size must_== 5
    }
    "have 1st step as cast on" in new steps {
      val step = steps(0)
      step.title must beText("Cast on main bed")
      step.description must beText("Perform a closed cast on on the main bed with red yarn on needles 1 until 40.")
      step.instructions.size must_== 1
    }
    "have 2nd step as add carriage" in new steps {
      val step = steps(1)
      step.title must beText("Add K")
      step.description must beText("Add K-carriage at the left.")
      step.instructions.size must_== 1
    }
    "have 3rd step as thread yarn" in new steps {
      val step = steps(2)
      step.title must beText("Thread Yarn to K")
      step.description must beText("Thread the red yarn into A on the K carriage.")
      step.instructions.size must_== 1
    }
    "have 4th step as knit one row" in new steps {
      val step = steps(3)
      step.title must beText("Knit 1 with K")
      step.description must beText("Knit 1 row with the K-carriage.")
      step.instructions.size must_== 1
      step.instructions(0).text must beText("Knit to right (last row).")
    }
    "have 5th step as cast off" in new steps {
      val step = steps(4)
      step.title must beText("Cast off main bed")
      step.description must beText("Perform a closed cast off on the main bed with red yarn for the marked needles.")
      step.instructions.size must_== 1
    }
  }

  "GuideParser(tenLinePlan)" should {
    trait steps extends WithApplication with tenLinePlan {
      val steps = GuideParser(tenLinePlan)
    }
    "have 5 steps" in new steps {
      steps.size must_== 5
    }
    "have 4th step as knit ten rows" in new steps {
      val step = steps(3)
      step.title must beText("Knit 10 with K")
      step.description must beText("Knit 10 rows with the K-carriage.")
      step.instructions.size must_== 10
      step.instructions(0).text must beText("Knit to right (9 rows remaining).")
      step.instructions(1).text must beText("Knit to left (8 rows remaining).")
      step.instructions(8).text must beText("Knit to right (one row remaining).")
      step.instructions(9).text must beText("Knit to left (last row).")
    }
    "have 5th step as cast off" in new steps {
      val step = steps(4)
      step.title must beText("Cast off main bed")
      step.description must beText("Perform a closed cast off on the main bed with red yarn for the marked needles.")
      step.instructions.size must_== 1
    }
  }

  "GuideParser(twoYarnPlan)" should {
    trait steps extends WithApplication with twoYarnPlan {
      val steps = GuideParser(twoYarnPlan)
    }
    "have 7 steps" in new steps {
      steps.size must_== 7
    }
    "have 3rd step as red thread yarn" in new steps {
      val step = steps(2)
      step.title must beText("Thread Yarn to K")
      step.description must beText("Thread the red yarn into A on the K carriage.")
      step.instructions.size must_== 1
    }
    "have 4th step as knit three rows" in new steps {
      val step = steps(3)
      step.title must beText("Knit 3 with K")
      step.description must beText("Knit 3 rows with the K-carriage.")
      step.instructions.size must_== 3
      step.instructions(0).text must beText("Knit to right (2 rows remaining).")
      step.instructions(1).text must beText("Knit to left (one row remaining).")
      step.instructions(2).text must beText("Knit to right (last row).")
    }
    "have 5th step as green thread yarn" in new steps {
      val step = steps(4)
      step.title must beText("Thread Yarn to K")
      step.description must beText("Thread the green yarn into A on the K carriage.")
      step.instructions.size must_== 1
    }
    "have 6th step as knit three rows" in new steps {
      val step = steps(5)
      step.title must beText("Knit 3 with K")
      step.description must beText("Knit 3 rows with the K-carriage.")
      step.instructions.size must_== 3
      step.instructions(0).text must beText("Knit to left (2 rows remaining).")
      step.instructions(1).text must beText("Knit to right (one row remaining).")
      step.instructions(2).text must beText("Knit to left (last row).")
    }
    "have 7th step as cast off" in new steps {
      val step = steps(6)
      step.title must beText("Cast off main bed")
      step.description must beText("Perform a closed cast off on the main bed with green yarn for the marked needles.")
      step.instructions.size must_== 1
    }
  }
}
