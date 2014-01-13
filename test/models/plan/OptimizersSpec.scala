package models.plan

import org.specs2.mutable.Specification
import org.specs2._
import models._
import models.planners._

class OptimizersSpec extends Specification {
  trait plans extends Yarns {
    val allNeedles = (n: Needle) => true
    val simpleLines = Plan(List(
      ClosedCastOn(Needle.atIndex(1), Needle.atIndex(40), red),
      AddCarriage(KCarriage, Left),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ThreadYarn(Some(red), None),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      ClosedCastOff(red, allNeedles)))

    val simpleLinesWithUnknittedSettings = Plan(List(
      ClosedCastOn(Needle.atIndex(1), Needle.atIndex(40), red),
      AddCarriage(KCarriage, Left),
      ChangeCarriageSettings(LCarriageSettings()),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ThreadYarn(Some(red), None),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ClosedCastOff(red, allNeedles)))

    val simpleLinesWithDuplicateSettings = Plan(List(
      ClosedCastOn(Needle.atIndex(1), Needle.atIndex(40), red),
      AddCarriage(KCarriage, Left),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ThreadYarn(Some(red), None),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      ClosedCastOff(red, allNeedles)))

    val simpleLinesWitUselessSettings = Plan(List(
      ClosedCastOn(Needle.atIndex(1), Needle.atIndex(40), red),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      AddCarriage(KCarriage, Left),
      ChangeCarriageSettings(LCarriageSettings()),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ThreadYarn(Some(red), None),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Right, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      KnitRow(KCarriage, Left, Some(AllNeedlesToB)),
      ChangeCarriageSettings(KCarriageSettings(KC2)),
      ClosedCastOff(red, allNeedles)))

    val plans = simpleLines ::
      simpleLinesWithUnknittedSettings ::
      simpleLinesWithDuplicateSettings :: Nil
  }

  "optimizers" should {
    def sameOutput(p: Plan) = {
      val unopt = p.run
      val opt = Plan(Optimizers.all(p.steps)).run
      unopt.isSuccess must beTrue
      unopt.map(_.output) must_== opt.map(_.output)
    }

    "not change result" in new plans {
      forall(plans)(sameOutput)
    }
  }

  "unknitted settings optimizer" should {
    "not change already optimal" in new plans {
      UnknittedSettingsOptimizer(simpleLines.steps) must_== simpleLines.steps
    }
    "remove unknitted change settings" in new plans {
      UnknittedSettingsOptimizer(simpleLinesWithUnknittedSettings.steps) must
        containTheSameElementsAs(simpleLines.steps)
    }
  }
  "duplicate settings optimizer" should {
    "not change already optimal" in new plans {
      DuplicateSettingsOptimizer(simpleLines.steps) must_== simpleLines.steps
    }
    "remove duplicate change settings" in new plans {
      DuplicateSettingsOptimizer(simpleLinesWithDuplicateSettings.steps) must
        containTheSameElementsAs(simpleLines.steps)
    }
  }
  "settings optimizers" should {
    "not change already optimal" in new plans {
      DuplicateSettingsOptimizer(simpleLines.steps) must_== simpleLines.steps
    }
    "remove useless change settings" in new plans {
      DuplicateSettingsOptimizer(
        UnknittedSettingsOptimizer(simpleLinesWithDuplicateSettings.steps)) must
        containTheSameElementsAs(simpleLines.steps)
    }
  }
}