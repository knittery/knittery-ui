package models.plan

import java.awt.Color
import org.scalameter.api._
import models.gauge.StandardGauge
import squants.space.LengthConversions._
import knittings.{Scarf, LaptopCase, Sock}
import models._

class OptimizerBenchmark extends PerformanceTest.Quickbenchmark {
  implicit val gauge = StandardGauge(34, 42, 5.tension)

  val tinyScarf = {
    def pattern(dim: Scarf.Dimensions) = IndexedSeq.fill(dim.width.approx, dim.height.approx)(Yarn("red", Color.red))
    Scarf.rectangular(4 cm, 4 cm, pattern)
  }
  val smallScarf = {
    def pattern(dim: Scarf.Dimensions) = IndexedSeq.fill(dim.width.approx, dim.height.approx)(Yarn("red", Color.red))
    Scarf.rectangular(10 cm, 10 cm, pattern)
  }
  val sock = Sock.europeanSize(40, Yarn("red", Color.red))
  val laptop = {
    val pattern = LaptopCase.checkerboardPattern(Yarn("red", Color.red), Yarn("blue", Color.blue), 2 cm)
    LaptopCase.form(10 cm, 20 cm, 0.5 cm, 5 cm, 1 cm, pattern)
  }

  val planners = Gen.enumeration("plans")(tinyScarf, smallScarf, laptop, sock)

  performance of "NoEffectStepOptimizer" in {
    measure method "apply" in {
      using(planners) in { planner =>
        planner.plan(NoEffectStepOptimizer)
      }
    }
  }

  performance of "OptimizePatternKnitting" in {
    measure method "apply" in {
      using(planners) in { planner =>
        planner.plan(OptimizePatternKnitting)
      }
    }
  }

  performance of "OptimizeRetires" in {
    measure method "apply" in {
      using(planners) in { planner =>
        planner.plan(OptimizeRetires)
      }
    }
  }

  performance of "OptimizeStepWithNoEffectOnFinalOutput" in {
    measure method "apply" in {
      using(planners) in { planner =>
        planner.plan(OptimizeStepWithNoEffectOnFinalOutput)
      }
    }
  }
}
