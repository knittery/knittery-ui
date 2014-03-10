package models.plan

import org.specs2.mutable.Specification
import org.specs2._
import models._
import models.planners._

class OptimizersSpec extends Specification {
  trait plans extends Yarns {
    val allNeedles = (n: Needle) => true

    val redPiece = YarnPiece(red)
    val greenPiece = YarnPiece(green)

    val simpleLines = Plan(List(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      AddCarriage(KCarriage, Left),
      ThreadYarnK(Some(redPiece), None),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      KnitRow(KCarriage, ToRight),
      KnitRow(KCarriage, ToLeft),
      ClosedCastOff(MainBed, redPiece, allNeedles)))

    val simpleLinesWithUnknittedSettings = Plan(List(
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
      ClosedCastOff(MainBed, redPiece, allNeedles)))

    val simpleLinesWithDuplicateSettings = Plan(List(
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
      ClosedCastOff(MainBed, redPiece, allNeedles)))

    val simpleLinesWitUselessSettings = Plan(List(
      ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
      ChangeKCarriageSettings(KCarriage.Settings(), KCarriage.SinkerPlate()),
      AddCarriage(KCarriage, Left),
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
      ClosedCastOff(MainBed, redPiece, allNeedles)))

    def evenOddPattern(n: Needle) = if (n.index % 2 == 0 || n.index >= 1 || n.index <= 40) NeedleToB else NeedleToD
    def oddEvenPattern(n: Needle) = if (n.index % 2 == 1 || n.index >= 1 || n.index <= 40) NeedleToB else NeedleToD
    def fourty(pos: NeedlePosition) = (n: Needle) => if (n.index >= 1 && n.index <= 40) pos else NeedleA

    val patternLines = Plan(List(
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
      ClosedCastOff(MainBed, redPiece, allNeedles)))

    val patternLinesWithManualNeedleSettings = {
      val line = fourty(NeedleB)
      Plan(List(
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
        ClosedCastOff(MainBed, redPiece, allNeedles)))
    }

    val plainKnittingK = {
      Plan(List(
        ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
        AddCarriage(KCarriage, Left),
        ThreadYarnK(Some(redPiece), None),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        KnitRow(KCarriage, ToLeft),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        KnitRow(KCarriage, ToLeft),
        KnitRow(KCarriage, ToRight, AllNeedlesToD),
        KnitRow(KCarriage, ToLeft)))
    }
    val plainKnittingKManualMovements = {
      val line = fourty(NeedleB)
      Plan(List(
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
        KnitRow(KCarriage, ToLeft)))
    }

    val plainKnittingKWithEAndH = {
      Plan(List(
        ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
        AddCarriage(KCarriage, Left),
        ThreadYarnK(Some(redPiece), None),
        ChangeKCarriageSettings(KCarriage.Settings(holdingCamLever = KCarriage.HoldingCamH), KCarriage.SinkerPlate()),
        KnitRow(KCarriage, ToRight),
        MoveNeedles(MainBed, fourty(NeedleE)),
        KnitRow(KCarriage, ToLeft),
        ClosedCastOff(MainBed, redPiece, allNeedles)))
    }
    val plainKnittingKWithEAndN_unoptimized = {
      Plan(List(
        ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
        AddCarriage(KCarriage, Left),
        ThreadYarnK(Some(redPiece), None),
        KnitRow(KCarriage, ToRight),
        MoveNeedles(MainBed, fourty(NeedleE)),
        KnitRow(KCarriage, ToLeft),
        ClosedCastOff(MainBed, redPiece, allNeedles)))
    }
    val plainKnittingKWithEAndN = {
      Plan(List(
        ClosedCastOn(MainBed, Needle.atIndex(1), Needle.atIndex(40), redPiece),
        AddCarriage(KCarriage, Left),
        ThreadYarnK(Some(redPiece), None),
        KnitRow(KCarriage, ToRight),
        KnitRow(KCarriage, ToLeft),
        ClosedCastOff(MainBed, redPiece, allNeedles)))
    }

    val plans = simpleLines ::
      simpleLinesWithUnknittedSettings ::
      simpleLinesWithDuplicateSettings ::
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
      val opt = Plan(Optimizers.all(p.steps)).run
      ("Plan fails: " + unopt) <==> (unopt.isSuccess must beTrue)
      opt.isSuccess must beTrue
      unopt.map(_.output) must_== opt.map(_.output)
      unopt.map(_.output2) must_== opt.map(_.output2)
    }

    "not change result" in new plans {
      forall(plans)(sameOutput)
    }
  }

  "unknitted settings optimizer" should {
    "not change already optimal" in new plans {
      UnknittedSettingsOptimizer(simpleLines.steps) must containTheSameElementsAs(simpleLines.steps)
    }
    "remove unknitted change settings" in new plans {
      UnknittedSettingsOptimizer(simpleLinesWithUnknittedSettings.steps) must
        containTheSameElementsAs(simpleLines.steps)
    }
  }
  "no effect step optimizer" should {
    "not change already optimal" in new plans {
      NoEffectStepOptimizer(simpleLines.steps) must containTheSameElementsAs(simpleLines.steps)
    }
    "remove duplicate change settings" in new plans {
      NoEffectStepOptimizer(simpleLinesWithDuplicateSettings.steps) must
        containTheSameElementsAs(simpleLines.steps)
    }
  }
  "settings optimizers" should {
    "not change already optimal" in new plans {
      NoEffectStepOptimizer(simpleLines.steps) must containTheSameElementsAs(simpleLines.steps)
    }
    "remove useless change settings" in new plans {
      NoEffectStepOptimizer(
        UnknittedSettingsOptimizer(simpleLinesWithDuplicateSettings.steps)) must
        containTheSameElementsAs(simpleLines.steps)
    }
  }

  "pattern knitting optimizer" should {
    "not change already optimal" in new plans {
      OptimizePatternKnitting(patternLines.steps) must containTheSameElementsAs(patternLines.steps)
    }
    "prevent manual needle movement" in new plans {
      OptimizePatternKnitting(patternLinesWithManualNeedleSettings.steps) must
        containTheSameElementsAs(patternLines.steps)
    }
  }

  "useless move needles optimization" should {
    "not change already optimal" in new plans {
      OptimizeStepWithNoEffect(plainKnittingK.steps) must
        containTheSameElementsAs(plainKnittingK.steps)
    }
    "remove useless move needles" in new plans {
      OptimizeStepWithNoEffect(plainKnittingKManualMovements.steps) must
        containTheSameElementsAs(plainKnittingK.steps)
    }
    "not optimize away movements to E with HoldingCam H" in new plans {
      OptimizeStepWithNoEffect(plainKnittingKWithEAndH.steps) must
        containTheSameElementsAs(plainKnittingKWithEAndH.steps)
    }
    "optimize away movements to E with HoldingCam N" in new plans {
      OptimizeStepWithNoEffect(plainKnittingKWithEAndN_unoptimized.steps) must
        containTheSameElementsAs(plainKnittingKWithEAndN.steps)
    }
  }
}