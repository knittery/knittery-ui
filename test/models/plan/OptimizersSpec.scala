package models.plan

import org.specs2.mutable.Specification
import models._


class OptimizersSpec extends Specification {
  trait plans extends Yarns {
    val allNeedles = (n: Needle) => true

    val redPiece = YarnPiece(red)
    val greenPiece = YarnPiece(green)

    def plan(steps: Step*) =
      Plan(steps).valueOr(e => throw new RuntimeException(s"Invalid test-plan: $e"))

    val simpleLines = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, allNeedles))

    val simpleLinesWithUnknittedSettings = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ClosedCastOff(MainBed, redPiece, allNeedles))

    val simpleLinesWithDuplicateSettings = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ThreadYarnK(Some(redPiece), None),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToRight),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToLeft),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToRight),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToLeft),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToRight),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, allNeedles))

    val simpleLinesWitUselessSettings = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      AddCarriage(LCarriage, Right),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ChangeLCarriageSettings(LCarriage.Settings()),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ThreadYarnK(Some(redPiece), None),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToRight),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToLeft),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToRight),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToLeft),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToRight),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToLeft),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ClosedCastOff(MainBed, redPiece, allNeedles))

    def evenOddPattern(n: Needle) = if (n.index % 2 == 0 || n.index >= 1 || n.index <= 40) NeedleToB else NeedleToD
    def oddEvenPattern(n: Needle) = if (n.index % 2 == 1 || n.index >= 1 || n.index <= 40) NeedleToB else NeedleToD
    def fourty(pos: NeedlePosition) = (n: Needle) => if (n.index >= 1 && n.index <= 40) pos else NeedleA

    val patternLines = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight, evenOddPattern),
      ThreadYarnK(Some(redPiece), Some(greenPiece)),
      ChangeKCarriageSettings(KCarriage.Settings(mc = true), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToLeft, oddEvenPattern),
      KnitRow(KCarriage, ToRight, evenOddPattern),
      KnitRow(KCarriage, ToLeft, oddEvenPattern),
      KnitRow(KCarriage, ToRight, evenOddPattern),
      KnitRow(KCarriage, ToLeft, oddEvenPattern),
      KnitRow(KCarriage, ToRight, evenOddPattern),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, allNeedles))

    val patternLinesWithManualNeedleSettings = {
      val line = fourty(NeedleB)
      plan(
        ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
        AddCarriage(KCarriage, Left),
        ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
        ThreadYarnK(Some(redPiece), None),
        KnitRow(KCarriage, ToRight, evenOddPattern),
        ThreadYarnK(Some(redPiece), Some(greenPiece)),
        ChangeKCarriageSettings(KCarriage.Settings(mc = true), KCarriage.SinkerPlate()),
        MoveNeedles(line, oddEvenPattern, true),
        KnitRow(KCarriage, ToLeft),
        MoveNeedles(line, oddEvenPattern, true),
        KnitRow(KCarriage, ToRight),
        MoveNeedles(line, oddEvenPattern, true),
        KnitRow(KCarriage, ToLeft),
        MoveNeedles(line, oddEvenPattern, true),
        KnitRow(KCarriage, ToRight),
        MoveNeedles(line, oddEvenPattern, true),
        KnitRow(KCarriage, ToLeft),
        MoveNeedles(line, oddEvenPattern, true),
        KnitRow(KCarriage, ToRight),
        MoveNeedles(line, oddEvenPattern, true),
        KnitRow(KCarriage, ToLeft),
        ClosedCastOff(MainBed, redPiece, allNeedles))
    }

    val plainKnittingK = {
      plan(
        ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
        AddCarriage(KCarriage, Left),
        ThreadYarnK(Some(redPiece), None),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        KnitRow(KCarriage, ToLeft),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        KnitRow(KCarriage, ToLeft),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        KnitRow(KCarriage, ToLeft))
    }
    val plainKnittingKManualMovements = {
      val line = fourty(NeedleB)
      plan(
        ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
        AddCarriage(KCarriage, Left),
        ThreadYarnK(Some(redPiece), None),
        MoveNeedles(MainBed, line),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        MoveNeedles(MainBed, line),
        KnitRow(KCarriage, ToLeft),
        MoveNeedles(MainBed, line),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        MoveNeedles(MainBed, line),
        KnitRow(KCarriage, ToLeft),
        MoveNeedles(MainBed, line),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        MoveNeedles(MainBed, line),
        KnitRow(KCarriage, ToLeft))
    }

    val plainKnittingKWithEAndH = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      ChangeKCarriageSettings(KCarriage.Settings(holdingCamLever = KCarriage.HoldingCamH), KCarriage.SinkerPlate()),
      KnitRow(KCarriage, ToRight),
      MoveNeedles(MainBed, fourty(NeedleE)),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, allNeedles))

    val plainKnittingKWithEAndN_unoptimized = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      MoveNeedles(MainBed, fourty(NeedleE)),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, allNeedles))

    val plainKnittingKWithEAndN = plan(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, allNeedles))

    val basePlan = Seq(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft))

    val endOfPlan = Seq(
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, _ => true))

    def wrapInPlan(steps: Step*) = {
      plan(basePlan ++ steps ++ endOfPlan: _*)
    }


    val plans = simpleLines ::
      simpleLinesWithUnknittedSettings ::
      simpleLinesWithDuplicateSettings ::
      simpleLinesWitUselessSettings ::
      patternLines ::
      patternLinesWithManualNeedleSettings ::
      plainKnittingK ::
      plainKnittingKManualMovements ::
      plainKnittingKWithEAndH ::
      plainKnittingKWithEAndN_unoptimized ::
      plainKnittingKWithEAndN ::
      Nil
  }

  "optimizers" should {
    def sameOutput(p: Plan) = {
      val unopt = p.run
      val opt = Optimizers.all(p).run
      (unopt.output must_== opt.output) and
        (unopt.output3D must_== opt.output3D)
    }

    "not change result" in new plans {
      forall(plans)(sameOutput)
    }
  }

  "no effect step optimizer" should {
    "not change already optimal" in new plans {
      NoEffectStepOptimizer(simpleLines).steps must containTheSameElementsAs(simpleLines.steps)
    }
    "remove duplicate change settings" in new plans {
      NoEffectStepOptimizer(simpleLinesWithDuplicateSettings).steps must
        containTheSameElementsAs(simpleLines.steps)
    }
    "remove useless change settings" in new plans {
      NoEffectStepOptimizer(simpleLinesWithDuplicateSettings).steps must
        containTheSameElementsAs(simpleLines.steps)
    }
  }

  "pattern knitting optimizer" should {
    "not change already optimal" in new plans {
      OptimizePatternKnitting(patternLines).steps must containTheSameElementsAs(patternLines.steps)
    }
    "prevent manual needle movement" in new plans {
      OptimizePatternKnitting(patternLinesWithManualNeedleSettings).steps must
        containTheSameElementsAs(patternLines.steps)
    }
  }

  "useless step optimization" should {
    "not change already optimal" in new plans {
      OptimizeStepWithNoEffectOnFinalOutput(plainKnittingK).steps must
        containTheSameElementsAs(plainKnittingK.steps)
    }
    "remove useless move needles" in new plans {
      OptimizeStepWithNoEffectOnFinalOutput(plainKnittingKManualMovements).steps must
        containTheSameElementsAs(plainKnittingK.steps)
    }
    "not optimize away movements to E with HoldingCam H" in new plans {
      OptimizeStepWithNoEffectOnFinalOutput(plainKnittingKWithEAndH).steps must
        containTheSameElementsAs(plainKnittingKWithEAndH.steps)
    }
    "optimize away movements to E with HoldingCam N" in new plans {
      OptimizeStepWithNoEffectOnFinalOutput(plainKnittingKWithEAndN_unoptimized).steps must
        containTheSameElementsAs(plainKnittingKWithEAndN.steps)
    }
  }

  "triple decker optimization" should {
    def toLeft(i: Int) = RetireNeedle(MainBed, Needle.atIndex(i), ToLeft)
    def toRight(i: Int) = RetireNeedle(MainBed, Needle.atIndex(i), ToRight)
    def withDouble(i: Int, dir: Direction) = RetireWithDouble(MainBed, Needle.atIndex(i), dir)
    def withTriple(i: Int, dir: Direction) = RetireWithTriple(MainBed, Needle.atIndex(i), dir)

    "merge together two adjunct retires to left when started with left" in new plans {
      val unopt = wrapInPlan(toLeft(39), toLeft(40))
      val opt = wrapInPlan(withDouble(39, ToLeft))
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
    "not merge together two adjunct retires to left when started with right" in new plans {
      val unopt = wrapInPlan(toLeft(40), toLeft(39))
      val opt = unopt
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }

    "merge together three adjunct retires to left when started with left" in new plans {
      val unopt = wrapInPlan(toLeft(38), toLeft(39), toLeft(40))
      val opt = wrapInPlan(withTriple(38, ToLeft))
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
    "not merge together three adjunct retires to left when started with right" in new plans {
      val unopt = wrapInPlan(toLeft(40), toLeft(39), toLeft(38))
      val opt = unopt
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
    "not merge together three adjunct retires to left when started with middle then right (only two)" in new plans {
      val unopt = wrapInPlan(toLeft(39), toLeft(40), toLeft(38))
      val opt = wrapInPlan(withDouble(39, ToLeft), toLeft(38))
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
    "not merge together three adjunct retires to left when started with middle then left (only two)" in new plans {
      val unopt = wrapInPlan(toLeft(39), toLeft(38), toLeft(40))
      val opt = wrapInPlan(withDouble(39, ToLeft), toLeft(38))
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }

    "merge together two adjunct retires to right when started with right" in new plans {
      val unopt = wrapInPlan(toRight(2), toRight(1))
      val opt = wrapInPlan(withDouble(1, ToRight))
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
    "not merge together two adjunct retires to right when started with left" in new plans {
      val unopt = wrapInPlan(toRight(1), toRight(2))
      val opt = unopt
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }

    "merge together three adjunct retires to right when started with right" in new plans {
      val unopt = wrapInPlan(toRight(3), toRight(2), toRight(1))
      val opt = wrapInPlan(withTriple(1, ToRight))
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
    "not merge together three adjunct retires to right when started with left" in new plans {
      val unopt = wrapInPlan(toRight(1), toRight(2), toRight(3))
      val opt = unopt
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
    "not merge together three adjunct retires to right when started with middle then left (only two)" in new plans {
      val unopt = wrapInPlan(toRight(2), toRight(1), toRight(3))
      val opt = wrapInPlan(withDouble(1, ToRight), toRight(3))
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
    "not merge together three adjunct retires to right when started with middle then right (only two)" in new plans {
      val unopt = wrapInPlan(toRight(2), toRight(3), toRight(1))
      val opt = wrapInPlan(withDouble(1, ToRight), toRight(3))
      OptimizeRetires(unopt).steps must containTheSameElementsAs(opt.steps)
    }
  }
}