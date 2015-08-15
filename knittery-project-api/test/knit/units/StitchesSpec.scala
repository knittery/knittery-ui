package knit.units

import squants.space.LengthConversions._
import knit._
import knit.gauge.StandardGauge
import org.specs2.mutable.Specification

class StitchesSpec extends Specification {
  "stitches" should {
    "implicitly convert from ints" in {
      val x = 12.stitches
      x.approx must_== 12
    }
    "add up with another stitches" in {
      val x = 12.stitches + 5.stitches
      x.approx must_== 17
    }
    "preserve partial stitches when adding" in {
      val x = (12.3).stitches + (1.3).stitches
      x.approx must_== 14
    }
    "preserve partial stitches when multiplying" in {
      val x = (12.3).stitches * 2
      x.approx must_== 25
    }
    "provide possibility to discard partial stitches" in {
      val x = (12.3).stitches.discardPartials * 2
      x.approx must_== 24
    }

    "convertable to length" in {
      implicit val g = StandardGauge(10, 33, 5 tension)
      val a = 123.stitches
      a.toLength must_== 123.cm
    }
  }
}
