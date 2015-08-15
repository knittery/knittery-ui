package products

import knit.Yarn
import knit.units.{Rows, Stitches}
import utils._

package object laptop {
  case class Patterns(front: Matrix[Yarn], back: Matrix[Yarn], lash: Matrix[Yarn])

  case class Dimensions(bodyWidth: Stitches, frontHeight: Rows, backHeight: Rows, lashWidth: Stitches, lashHeight: Rows) {
    def checkPatterns(ps: Patterns) = {
      require(ps.front.size == frontHeight.approx, "wrong front height")
      require(ps.front.head.size == bodyWidth.approx, "wrong front width")
      require(ps.back.size == backHeight.approx, "wrong back height")
      require(ps.back.head.size == bodyWidth.approx, "wrong back width")
      require(ps.lash.size == lashHeight.approx, s"wrong lash height: ${ps.lash.size} instead of ${lashHeight.approx}")
      require(ps.lash.head.size == lashWidth.approx,
        s"wrong lash width: ${ps.lash.head.size} instead of ${lashWidth.approx}")
      ps
    }
  }
}
