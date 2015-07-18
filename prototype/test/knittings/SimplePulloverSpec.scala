package knittings

import models.units._
import org.specs2.mutable.Specification

class SimplePulloverSpec extends Specification {

  "rowsToDecrease" should {
    "when decrease 2 over 30 do it at 10 and 20" in {
      val decs = SimplePullover.rowsWithDecreases(30.rows, 2.stitches)
      decs.size must_== 30
      decs.toList must_== List(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2).map(_.stitches)
    }

    "when increase 2 over 30 do it at 10 and 20" in {
      val decs = SimplePullover.rowsWithDecreases(30.rows, -2.stitches)
      decs.toList must_== List(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -2, -2, -2, -2, -2, -2, -2, -2, -2, -2).map(_.stitches)
    }
  }
}
