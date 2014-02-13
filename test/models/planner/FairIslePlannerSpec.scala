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
    planner.plan().valueOr(e => throw new RuntimeException("Could not build plan: " + e))
  }
  def runPlan(planner: Planner) = {
    planFor(planner).run.valueOr(e => throw new RuntimeException("Could run plan: " + e))
  }

  def knittedRow(stitch: Stitch*)(default: Stitch = EmptyStitch) = { (n: Needle) =>
    stitch.lift(n.index).getOrElse(default)
  }.all

  "FairIslePlanner.singleBed" should {
    "process 5x4 red/green checkerboard pattern (after cast-on)" in new Patterns {
      val redStart = YarnPiece(red)
      val planner = Cast.onClosed(needle0, needle4, red) >>
        Basics.knitRowWithK(yarnA = Some(redStart)) >>
        FairIslePlanner.singleBed(checkerboard(Needle.count, 4)) >>
        Cast.offClosed(redStart)
      val state = runPlan(planner)
      val out = state.output
      //println(out.patternString)
      out.height must_== 8
      out.rows(0) must_== knittedRow(CastOnStitch(red), CastOnStitch(red), CastOnStitch(red), CastOnStitch(red), CastOnStitch(red))(NoStitch)
      out.rows(1) must_== knittedRow(PlainStitch(red), PlainStitch(red), PlainStitch(red), PlainStitch(red), PlainStitch(red))(EmptyStitch)
      out.rows(2) must_== knittedRow(PlainStitch(red), PlainStitch(red), PlainStitch(red), PlainStitch(red), PlainStitch(red))(EmptyStitch)
      out.rows(3) must_== knittedRow(PlainStitch(red), PlainStitch(green), PlainStitch(red), PlainStitch(green), PlainStitch(red))(EmptyStitch)
      out.rows(4) must_== knittedRow(PlainStitch(green), PlainStitch(red), PlainStitch(green), PlainStitch(red), PlainStitch(green))(EmptyStitch)
      out.rows(5) must_== knittedRow(PlainStitch(red), PlainStitch(green), PlainStitch(red), PlainStitch(green), PlainStitch(red))(EmptyStitch)
      out.rows(6) must_== knittedRow(PlainStitch(green), PlainStitch(red), PlainStitch(green), PlainStitch(red), PlainStitch(green))(NoStitch)
      out.rows(7) must_== knittedRow(CastOffStitch(red), CastOffStitch(red), CastOffStitch(red), CastOffStitch(red), CastOffStitch(red))(NoStitch)
    }
  }
}