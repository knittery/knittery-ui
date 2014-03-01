package utils.graph

import org.scalameter.api._
import models._
import models.planners.Examples
import javax.imageio.ImageIO
import java.io.File
import scalaz.Validation
import models.plan.Optimizers

class SpringLayoutBenchmark extends PerformanceTest.Quickbenchmark with Yarns {
  case class Plan(name: String, planner: models.plan.PlannerM[_]) {
    val plan = planner.plan(Optimizers.no).valueOr(e => throw new RuntimeException(e))
    val result = plan.run.valueOr(e => throw new RuntimeException(e.error))
    val graph = result.output2.asGraph
    override def toString = s"$name (${graph.nodes.size})"
  }

  val plans = Gen.enumeration("Plans")(
    //Plan("pattern", Examples.imageRag(ImageIO.read(new File("example.png")))),
    //Plan("normalSock", Examples.sock(30, 60, 40, YarnPiece(red))),
    Plan("tinySock", Examples.sock(10, 10, 10, YarnPiece(red))),
    Plan("smallerSock", Examples.sock(12, 20, 20, YarnPiece(red))),
    Plan("smallSock", Examples.sock(20, 25, 20, YarnPiece(red))))

  val atStep = Gen.enumeration("atStep")(1)

  def layouts[N](f: Plan => IncrementalLayout[N]) = for {
    plan <- plans
    preps <- atStep
  } yield {
    (1 to preps).foldLeft(f(plan))((l, _) => l.improve)
  }

  val box = Box(1000)

  println("Done initializing")

  performance of "SpringLayout" in {
    measure method "improve" in {
      val ls = layouts(p => SpringLayout.apply(p.graph, box))
      using(ls) in { l =>
        l.improve()
      }
    }
  }

  performance of "ImmutableSpringLayout" in {
    measure method "improve" in {
      val ls = layouts(p => ImmutableSpringLayout.apply(p.graph, box))
      using(ls) in { l =>
        l.improve()
      }
    }
  }

  performance of "SpringBarnesHutLayout" in {
    measure method "improve" in {
      val ls = layouts(p => SpringBarnesHutLayout.apply(p.graph, box))
      using(ls) in { l =>
        l.improve()
      }
    }
  }
}