package products.laptop

import knit.Yarn
import knit.units._
import utils._

import scala.util.Random


/** Random gradient pattern. */
object Gradient {
  def apply(yarnTop: Yarn, yarnBottom: Yarn, yarnForLash: Yarn, seed: Long = 0) = (dims: Dimensions) => {
    val random = new Random(seed)
    val border = 2.stitches

    def gradientPattern(width: Int, height: Int, top: Yarn, bottom: Yarn) = {
      (0 until height).map(_.toDouble / height).map { prob =>
        IndexedSeq.fill(width)(if (random.nextDouble() < prob) bottom else top)
      }
    }
    def addBorder(to: Matrix[Yarn]) = {
      val b = IndexedSeq.fill(border.approx)(yarnTop)
      to.map(b ++ _ ++ b)
    }

    val width = dims.bodyWidth - border * 2
    val front = addBorder(gradientPattern(width.approx, dims.frontHeight.approx, yarnTop, yarnBottom))
    val back = addBorder(gradientPattern(width.approx, dims.backHeight.approx, yarnTop, yarnBottom))
    val lash = IndexedSeq.fill(dims.lashHeight.approx, dims.lashWidth.approx)(yarnForLash)
    Patterns(front, back, lash)
  }

}
