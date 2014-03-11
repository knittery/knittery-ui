package utils.graph

import javax.imageio.ImageIO
import java.io.File
import scalaz.Validation
import org.scalameter.api._
import models._
import models.planners.Examples
import models.plan.Optimizers
import utils.vector._

object SpringLayoutBenchmark extends PerformanceTest.Quickbenchmark with Yarns {
  case class Plan(name: String, planner: models.plan.PlannerM[_]) {
    val plan = planner.plan(Optimizers.no).valueOr(e => throw new RuntimeException(e))
    val result = plan.run.valueOr(e => throw new RuntimeException(e.error))
    val graph = result.output3D.asGraph
    override def toString = s"$name (${graph.nodes.size})"
  }

  val smallPlans = Gen.enumeration("Plans")(
    Plan("smallSock", Examples.sock(20, 25, 20, YarnPiece(red))),
    Plan("smallerSock", Examples.sock(12, 20, 15, YarnPiece(red))),
    Plan("tinySock", Examples.sock(10, 10, 10, YarnPiece(red))))

  val allPlans = Gen.enumeration("Plans")(
    Plan("bigSock", Examples.sock(40, 100, 60, YarnPiece(red))),
    Plan("normalSock", Examples.sock(30, 60, 40, YarnPiece(red))),
    Plan("smallSock", Examples.sock(20, 25, 20, YarnPiece(red))),
    Plan("smallerSock", Examples.sock(12, 20, 15, YarnPiece(red))),
    Plan("tinySock", Examples.sock(10, 10, 10, YarnPiece(red))))

  val atStep = Gen.enumeration("atStep")(1)

  def layoutsSmall[N](f: Plan => IncrementalLayout[N]) = for {
    plan <- smallPlans
    preps <- atStep
  } yield {
    (1 to preps).foldLeft(f(plan))((l, _) => l.improve)
  }
  def layouts[N](f: Plan => IncrementalLayout[N]) = for {
    plan <- allPlans
    preps <- atStep
  } yield {
    (1 to preps).foldLeft(f(plan))((l, _) => l.improve)
  }

  val box = Box3(1000)

  println("Done initializing")

  performance of "SpringLayout" in {
    measure method "improve" in {
      val ls = layoutsSmall(p => SpringLayout.apply(p.graph, box))
      using(ls) in { l =>
        l.improve
      }
    }
  }

  performance of "ImmutableSpringLayout" in {
    measure method "improve" in {
      val ls = layoutsSmall(p => ImmutableSpringLayout.apply(p.graph, box))
      using(ls) in { l =>
        l.improve
      }
    }
  }

  performance of "ImmutableParallelSpringLayout" in {
    measure method "improve" in {
      val ls = layouts(p => ImmutableParallelSpringLayout.apply(p.graph, box))
      using(ls) in { l =>
        l.improve
      }
    }
  }

  performance of "SpringBarnesHutLayout" in {
    measure method "improve with theta 0" in {
      val ls = layoutsSmall(p => SpringBarnesHutLayout.apply(p.graph, box, 0d))
      using(ls) in { l =>
        l.improve
      }
    }
    measure method "improve with theta 0.5" in {
      val ls = layouts(p => SpringBarnesHutLayout.apply(p.graph, box, 0.5d))
      using(ls) in { l =>
        l.improve
      }
    }
    measure method "improve with theta 0.7" in {
      val ls = layouts(p => SpringBarnesHutLayout.apply(p.graph, box, 0.7d))
      using(ls) in { l =>
        l.improve
      }
    }
    measure method "improve with theta 0.7 after 200 steps" in {
      val ls = layouts(p => SpringBarnesHutLayout.apply(p.graph, box, 0.7d).improves(200))
      using(ls) in { l =>
        l.improve
      }
    }
    measure method "improve with theta 1" in {
      val ls = layouts(p => SpringBarnesHutLayout.apply(p.graph, box, 1))
      using(ls) in { l =>
        l.improve
      }
    }
  }
}