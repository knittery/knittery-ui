package models.units

import squants.space.LengthConversions._
import models._
import models.gauge.StandardGauge
import org.specs2.mutable.Specification

class RowsSpec extends Specification {
   "rows" should {
     "implicitly convert from ints" in {
       val x = 12.rows
       x.approx must_== 12
     }
     "add up with another rows" in {
       val x = 12.rows + 5.rows
       x.approx must_== 17
     }
     "preserve partial rows when adding" in {
       val x = (12.3).rows + (1.3).rows
       x.approx must_== 14
     }
     "preserve partial rows when multiplying" in {
       val x = (12.3).rows * 2
       x.approx must_== 25
     }
     "provide possibility to discard partial rows" in {
       val x = (12.3).rows.discardPartials * 2
       x.approx must_== 24
     }


     "convertable to length" in {
       implicit val g = StandardGauge(33, 20, 5 tension)
       val a = 124.rows
       a.toLength must_== 62.cm
     }
   }
 }
