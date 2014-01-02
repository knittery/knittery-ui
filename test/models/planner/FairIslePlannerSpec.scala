package models.planner

import java.awt.Color
import scalaz._
import Scalaz._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import models._
import models.plan._
import models.planners._
import utils._

class FairIslePlannerSpec extends Specification {
  trait Yarns extends Scope {
    val red = Yarn("red", Color.red)
    val green = Yarn("green", Color.green)
  }
  trait Patterns extends Yarns {
    def checkerboard(w: Int, h: Int): Matrix[Yarn] = {
      val s = Stream.from(0).map(i => if (i % 2 == 0) red else green)
      (0 until h).map { i =>
        val c = if (i % 2 == 0) s else s.drop(1)
        c.take(w).toIndexedSeq
      }
    }

    val needle0 = Needle.atIndex(0)
    val needle4 = Needle.atIndex(4)
    val needle9 = Needle.atIndex(9)
  }

  def planFor(planner: Planner) = {
    planner.plan.
      fold(e => throw new RuntimeException("Could not build plan: " + e), identity)
  }
  def runPlan(planner: Planner) = {
    planFor(planner).run.
      fold(e => throw new RuntimeException("Could run plan: " + e), identity)
  }

  def knittedRow(stich: Stich*) = {
    val stiches = stich ++ Stream.fill(Needle.count - stich.size)(NoStich)
    KnittedRow(stiches)
  }

  "FairIslePlanner.singleBed" should {
    "process 5x4 red/green checkerboard pattern (after cast-on)" in new Patterns {

      val planner = Cast.onClosed(needle0, needle4, red) >>
        FairIslePlanner.singleBed(checkerboard(Needle.count, 4), red) >>
        Cast.offClosed(red)
      val state = runPlan(planner)
      val out = state.output
      out.height must_== 8
      out.rows(0) must_== knittedRow(CastOnStich(red), CastOnStich(red), CastOnStich(red), CastOnStich(red), CastOnStich(red))
      out.rows(1) must_== knittedRow(PlainStich(red), PlainStich(red), PlainStich(red), PlainStich(red), PlainStich(red))
      out.rows(2) must_== knittedRow(PlainStich(red), PlainStich(red), PlainStich(red), PlainStich(red), PlainStich(red))
      out.rows(3) must_== knittedRow(PlainStich(red), PlainStich(green), PlainStich(red), PlainStich(green), PlainStich(red))
      out.rows(4) must_== knittedRow(PlainStich(green), PlainStich(red), PlainStich(green), PlainStich(red), PlainStich(green))
      out.rows(5) must_== knittedRow(PlainStich(red), PlainStich(green), PlainStich(red), PlainStich(green), PlainStich(red))
      out.rows(6) must_== knittedRow(PlainStich(green), PlainStich(red), PlainStich(green), PlainStich(red), PlainStich(green))
      out.rows(7) must_== knittedRow(CastOffStich(red), CastOffStich(red), CastOffStich(red), CastOffStich(red), CastOffStich(red))
    }

  }
}