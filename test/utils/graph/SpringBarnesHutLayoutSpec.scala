package utils.graph

import org.specs2.mutable.Specification
import org.specs2.matcher.Matcher
import org.specs2.specification.Scope
import models._
import models.plan._
import models.planners.Examples
import org.specs2.matcher.Expectable
import utils.vector._

class SpringBarnesHutLayoutSpec extends Specification {
  val boundaries = Box(2000)
  val baseEpsilon = (boundaries.size / 10000d).toVector3

  object P extends Yarns {
    def planner = Examples.sock(12, 20, 20, YarnPiece(red))
    def plan = planner.plan(Optimizers.no).valueOr(e => throw new RuntimeException(s"Invalid plan: $e"))
    def finalState = plan.run.valueOr(e => throw new RuntimeException(s"Invalid plan: $e"))
    val graph = finalState.output2.asGraph
    def nodes = graph.nodes.map(_.value)

    val initialPositions = nodes.map(n => (n, Vector3.random(boundaries).toMutable)).toMap
    val step_0 = ImmutableSpringLayout(graph, initialPositions)
    val step_1 = step_0.improve
    val step_10 = step_0.improve(10)
  }

  def beNear(ref: Position, epsilon: Vector3): Matcher[Position] = {
    beCloseTo(ref.x, epsilon.x) ^^ ((a: Position) => a.x) and
      beCloseTo(ref.y, epsilon.y) ^^ ((a: Position) => a.y) and
      beCloseTo(ref.z, epsilon.z) ^^ ((a: Position) => a.z)
  }
  def beLayoutedSimilarTo[N](to: Layout[N], forNodes: Traversable[N], epsilon: Vector3): Matcher[Layout[N]] = new Matcher[Layout[N]] {
    def apply[S <: Layout[N]](t: Expectable[S]) = {
      val pairs = forNodes.map(n => (to(n), t.value(n)))
      val a = forall(pairs) {
        case (should, is) =>
          is must beNear(should, epsilon).updateMessage(m => s"$is not close to $should (e=$epsilon)")
      }
      result(a.isSuccess, a.message, a.message, t)
    }
  }

  "Barnes-Hut spring layout with theta = 0" should {
    val baseLayout = SpringBarnesHutLayout(P.graph, P.initialPositions, 0d)
    "perform first step of small sock layouting very similar to spring layout" in {
      val afterFirst = baseLayout.improve
      afterFirst must beLayoutedSimilarTo(P.step_1, P.nodes, baseEpsilon)
    }
    "perform 10 steps of small sock layouting very similar to spring layout" in {
      val afterFirst = baseLayout.improve(10)
      afterFirst must beLayoutedSimilarTo(P.step_10, P.nodes, baseEpsilon * 10)
    }
  }
  "Barnes-Hut spring layout with theta = 0.3" should {
    val baseLayout = SpringBarnesHutLayout(P.graph, P.initialPositions, 0.3d)
    "perform first step of small sock layouting very similar to spring layout" in {
      val afterFirst = baseLayout.improve
      afterFirst must beLayoutedSimilarTo(P.step_1, P.nodes, baseEpsilon)
    }
    "perform 10 steps of small sock layouting very similar to spring layout" in {
      val afterFirst = baseLayout.improve(10)
      afterFirst must beLayoutedSimilarTo(P.step_10, P.nodes, baseEpsilon * 10)
    }
  }

}