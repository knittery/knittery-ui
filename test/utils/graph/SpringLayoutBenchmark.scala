package utils.graph

import org.scalameter.api._
import models._
import models.planners.Examples
import javax.imageio.ImageIO
import java.io.File
import scalaz.Validation

class SpringLayoutBenchmark extends PerformanceTest.Quickbenchmark with Yarns {
  case class Plan(name: String, plan: models.plan.Plan) {
    val result = plan.run.valueOr(e => throw new RuntimeException(e.error))
    val graph = result.output2.asGraph
    override def toString = name
  }
  object Plan {
    def apply(name: String, plan: Validation[String, models.plan.Plan]): Plan =
      Plan(name, plan.valueOr(e => throw new RuntimeException(e)))
  }
  //  val sheet = Examples.imageRag(ImageIO.read(new File("example.png"))).plan()

  val plans = Gen.enumeration("Plans")(
    //Plan("normalSock", Examples.sock(30, 60, 40, YarnPiece(red))),
    Plan("tinySock", Examples.sock(10, 10, 10, YarnPiece(red))),
    Plan("smallSock", Examples.sock(12, 20, 15, YarnPiece(red))))

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