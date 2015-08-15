package knit.planner

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import knit._
import knit.plan._
import knit.planners._
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
    planFor(planner).run
  }

  def knittedRow(stitch: Stitch*)(default: Stitch = EmptyStitch) = { (n: Needle) =>
    stitch.lift(n.index).getOrElse(default)
  }.all

  "FairIslePlanner.singleBed" should {
    "process 5x4 red/green checkerboard pattern (after cast-on)" in new Patterns {
      val redStart = YarnPiece(red)
      val planner = Cast.onClosed(MainBed, needle0, needle4, red) >>
        Basics.knitRowWithK(yarnA = Some(redStart)) >>
        FairIslePlanner.singleBed(checkerboard(Needle.count, 4)) >>
        Cast.offClosed(MainBed, redStart)
      val state = runPlan(planner)
      val out = state.output
      out.height must_== 8
      out.mainBed.rows.head must_== knittedRow(CastOnStitch(red), CastOnStitch(red), CastOnStitch(red), CastOnStitch(red), CastOnStitch(red))(NoStitch)
      out.mainBed.rows(1) must_== knittedRow(PlainStitch(red), PlainStitch(red), PlainStitch(red), PlainStitch(red), PlainStitch(red))(EmptyStitch)
      out.mainBed.rows(2) must_== knittedRow(PlainStitch(red), PlainStitch(red), PlainStitch(red), PlainStitch(red), PlainStitch(red))(EmptyStitch)
      out.mainBed.rows(3) must_== knittedRow(PlainStitch(red), PlainStitch(green), PlainStitch(red), PlainStitch(green), PlainStitch(red))(EmptyStitch)
      out.mainBed.rows(4) must_== knittedRow(PlainStitch(green), PlainStitch(red), PlainStitch(green), PlainStitch(red), PlainStitch(green))(EmptyStitch)
      out.mainBed.rows(5) must_== knittedRow(PlainStitch(red), PlainStitch(green), PlainStitch(red), PlainStitch(green), PlainStitch(red))(EmptyStitch)
      out.mainBed.rows(6) must_== knittedRow(PlainStitch(green), PlainStitch(red), PlainStitch(green), PlainStitch(red), PlainStitch(green))(EmptyStitch)
      out.mainBed.rows(7) must_== knittedRow(CastOffStitch(red), CastOffStitch(red), CastOffStitch(red), CastOffStitch(red), CastOffStitch(red))(EmptyStitch)
    }
  }
}