package connector

import org.specs2.mutable.Specification
import models._

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
      parser("@0\t<-\tK\t<") must_== Some(PositionUpdate(CarriageLeft(0), Left, Some(KCarriage)))
    }
    "parse pattern with L carriage over needle 45" in {
      val pos = CarriageOverNeedles(Needle.atIndex(45))
      parser("@45\t->\tL\t_") must_== Some(PositionUpdate(pos, Right, Some(LCarriage)))
    }
    "parse pattern with L carriage over needle 199" in {
      val pos = CarriageOverNeedles(Needle.atIndex(199))
      parser("@199\t->\tL\t_") must_== Some(PositionUpdate(pos, Right, Some(LCarriage)))
    }
    "parse pattern with G carriage moved over the right" in {
      parser("@0\t->\tG\t>") must_== Some(PositionUpdate(CarriageRight(0), Right, Some(GCarriage)))
    }
    "parse pattern with some stuff appended" in {
      parser("@0\t<-\tK\t<\tbla\tu") must_== Some(PositionUpdate(CarriageLeft(0), Left, Some(KCarriage)))
    }
  }

}