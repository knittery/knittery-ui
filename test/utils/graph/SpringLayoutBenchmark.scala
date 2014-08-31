package utils.graph

import org.scalameter.api._
import ch.inventsoft.graph.vector._
import ch.inventsoft.graph.layout._
import ch.inventsoft.graph.layout.spring._
import models._
import models.units._
import models.plan.Optimizers
import knittings.Sock

object SpringLayoutBenchmark extends PerformanceTest.Quickbenchmark with Yarns {
  case class Plan(name: String, planner: models.plan.PlannerM[_]) {
    val plan = planner.plan(Optimizers.no).valueOr(e => throw new RuntimeException(e))
    val result = plan.run
    val graph = result.output3D.asGraph
    override def toString = s"$name (${graph.nodes.size})"
  }

  val smallPlans = Gen.enumeration("Plans")(
    Plan("smallSock", Sock(20.stitches, 25.rows, 20.rows, red)),
    Plan("smallerSock", Sock(12.stitches, 20.rows, 15.rows, red)),
    Plan("tinySock", Sock(10.stitches, 10.rows, 10.rows, red)))

  val allPlans = Gen.enumeration("Plans")(
    Plan("bigSock", Sock(40.stitches, 100.rows, 60.rows, red)),
    Plan("normalSock", Sock(30.stitches, 60.rows, 40.rows, red)),
    Plan("smallSock", Sock(20.stitches, 25.rows, 20.rows, red)),
    Plan("smallerSock", Sock(12.stitches, 20.rows, 15.rows, red)),
    Plan("tinySock", Sock(10.stitches, 10.rows, 10.rows, red)))

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

  performance of "ParallelSpringLayout" in {
    measure method "improve" in {
      val ls = layouts(p => ParallelSpringLayout.apply(p.graph, box))
      using(ls) in { l =>
        l.improve
      }
    }
  }

  performance of "BarnesHutLayout" in {
    measure method "improve with theta 0" in {
      val ls = layoutsSmall(p => BarnesHutLayout.apply(p.graph, box, 0d))
      using(ls) in { l =>
        l.improve
      }
    }
    measure method "improve with theta 0.5" in {
      val ls = layouts(p => BarnesHutLayout.apply(p.graph, box, 0.5d))
      using(ls) in { l =>
        l.improve
      }
    }
    measure method "improve with theta 0.7" in {
      val ls = layouts(p => BarnesHutLayout.apply(p.graph, box, 0.7d))
      using(ls) in { l =>
        l.improve
      }
    }
    measure method "improve with theta 0.7 after 200 steps" in {
      val ls = layouts(p => BarnesHutLayout.apply(p.graph, box, 0.7d).improves(200))
      using(ls) in { l =>
        l.improve
      }
    }
    measure method "improve with theta 1" in {
      val ls = layouts(p => BarnesHutLayout.apply(p.graph, box, 1))
      using(ls) in { l =>
        l.improve
      }
    }
  }
}