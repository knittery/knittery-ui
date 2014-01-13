package models.connector

import org.specs2.mutable.Specification
import org.specs2.matcher._
import models._
import Connector._

class BrotherConnectorSpec extends Specification {

  "brother parser" should {
    import BrotherConnector._

    "parse None on empty input" in {
      parser("") must_== None
    }
    "parse None on invalid input" in {
      parser("blabla") must_== None
      parser("123") must_== None
    }
    "parse pattern with K carriage moved over the left" in {
      parser("@\t0\t-1\t<-\tK\t<") must_== Some(PositionUpdate(CarriageLeft(23), Left, Some(KCarriage)))
    }
    "parse pattern with L carriage over needle 45" in {
      val pos = CarriageOverNeedles(Needle.atIndex(45))
      parser("@\t45\t45\t->\tL\t_") must_== Some(PositionUpdate(pos, Right, Some(LCarriage)))
    }
    "parse pattern with L carriage over needle 199" in {
      val pos = CarriageOverNeedles(Needle.atIndex(199))
      parser("@\t199\t199\t->\tL\t_") must_== Some(PositionUpdate(pos, Right, Some(LCarriage)))
    }
    "parse pattern with G carriage moved over the right" in {
      parser("@\t199\t220\t->\tG\t>") must_== Some(PositionUpdate(CarriageRight(3), Right, Some(GCarriage)))
    }
    "parse pattern with some stuff appended" in {
      parser("@\t0\t0\t<-\tK\t<\tbla\tu") must_== Some(PositionUpdate(CarriageLeft(24), Left, Some(KCarriage)))
      parser("@\t0\t-24\t<-\tK\t<\tbla\tu") must_== Some(PositionUpdate(CarriageLeft(0), Left, Some(KCarriage)))
      parser("@\t0\t-40\t<-\tK\t<\tbla\tu") must_== Some(PositionUpdate(CarriageLeft(0), Left, Some(KCarriage)))
    }
  }

  "NeedleEncoder" should {
    case class matchPattern(expect: Needle => NeedleAction) extends Matcher[Needle => NeedleAction]() {
      def apply[S <: (Needle => NeedleAction)](t: Expectable[S]) = {
        val v = t.value
        val diff = Needle.all.zipWithIndex.collect {
          case (n, i) if (expect(n) != v(n)) => i
        }
        result(diff.isEmpty, "ok", "different pattern at: " + diff.mkString(", "), t)
      }
    }
    def samePattern(p1: Needle => NeedleAction, p2: Needle => NeedleAction) =
      Needle.all.forall(n => p1(n) == p2(n))

    "serialize & parse all one needle patterns" in {
      (0 until Needle.count).map { x =>
        def pattern(n: Needle) = if (n.index == x) NeedleToB else NeedleToD
        val string = NeedleEncoder.asString(pattern)
        string.length must_!= 0
        val p = NeedleEncoder.fromString(string)
        p must matchPattern(pattern)
      }
    }
    "serialize & parse all inverse-one needle patterns" in {
      (0 until Needle.count).map { x =>
        def pattern(n: Needle) = if (n.index == x) NeedleToD else NeedleToB
        val string = NeedleEncoder.asString(pattern)
        string.length must_!= 0
        val p = NeedleEncoder.fromString(string)
        p must matchPattern(pattern)
      }
    }

    "serialize & parse all B needle patterns" in {
      def pattern(n: Needle) = NeedleToB
      val string = NeedleEncoder.asString(pattern)
      string.length must_!= 0
      val p = NeedleEncoder.fromString(string)
      p must matchPattern(pattern)
    }
    "serialize & parse all D needle patterns" in {
      def pattern(n: Needle) = NeedleToD
      val string = NeedleEncoder.asString(pattern)
      string.length must_!= 0
      val p = NeedleEncoder.fromString(string)
      p must matchPattern(pattern)
    }

    "serialize all needles to B to 50 times F" in {
      def pattern(n: Needle) = NeedleToB
      NeedleEncoder.asString(pattern) must_== "f" * 50
    }
    "serialize all needles to B to 50 times 0" in {
      def pattern(n: Needle) = NeedleToD
      NeedleEncoder.asString(pattern) must_== "0" * 50
    }

    "parse 50 times f to all B" in {
      NeedleEncoder.fromString("f" * 50) must matchPattern(AllNeedlesToB)
    }
    "parse 50 times F to all B" in {
      NeedleEncoder.fromString("F" * 50) must matchPattern(AllNeedlesToB)
    }

    "parse 50 times 0 to all D" in {
      NeedleEncoder.fromString("0" * 50) must matchPattern(AllNeedlesToD)
    }

    "serialize needle 0 B all other D to 80000..." in {
      def pattern(n: Needle) = if (n.index == 0) NeedleToB else NeedleToD
      NeedleEncoder.asString(pattern) must_== "8" + ("0" * 49)
    }
    "serialize needle 1 B all other D to 40000..." in {
      def pattern(n: Needle) = if (n.index == 1) NeedleToB else NeedleToD
      NeedleEncoder.asString(pattern) must_== "4" + ("0" * 49)
    }
    "serialize needle 2 B all other D to 20000..." in {
      def pattern(n: Needle) = if (n.index == 2) NeedleToB else NeedleToD
      NeedleEncoder.asString(pattern) must_== "2" + ("0" * 49)
    }
    "serialize needle 3 B all other D to 10000..." in {
      def pattern(n: Needle) = if (n.index == 3) NeedleToB else NeedleToD
      NeedleEncoder.asString(pattern) must_== "1" + ("0" * 49)
    }
    "serialize needle 4 B all other D to 08000..." in {
      def pattern(n: Needle) = if (n.index == 4) NeedleToB else NeedleToD
      NeedleEncoder.asString(pattern) must_== "08" + ("0" * 48)
    }
    "serialize needle 199 B all other D to 08000..." in {
      def pattern(n: Needle) = if (n.index == 199) NeedleToB else NeedleToD
      NeedleEncoder.asString(pattern) must_== ("0" * 49) + "1"
    }

    "parse 8000... to needle 0 B all other D" in {
      def pattern(n: Needle) = if (n.index == 0) NeedleToB else NeedleToD
      NeedleEncoder.fromString("8" + ("0" * 49)) must matchPattern(pattern)
    }
    "parse 00..01 to needle 199 B all other D" in {
      def pattern(n: Needle) = if (n.index == 199) NeedleToB else NeedleToD
      NeedleEncoder.fromString(("0" * 49) + "1") must matchPattern(pattern)
    }
  }

}