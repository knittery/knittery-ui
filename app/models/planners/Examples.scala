package models.planners

import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import scala.util.Random
import scalaz._
import Scalaz._
import squants.space.Length
import squants.space.LengthConversions._
import utils._
import models.gauge.Gauge
import models._
import models.plan._
import models.KCarriage.TensionDial

object Examples {

  def handyHuelle(img: BufferedImage, background: Yarn, tension: TensionDial): Planner = for {
    width <- Planner.precondidtions(_ => img.getWidth)
    height <- Planner.precondidtions(_ => img.getHeight)
    first <- Planner.precondidtions { _ =>
      require(width <= Needle.count - 1)
      Needle.middle - (width / 2)
    }
    last = first + width
    backgroundPiece = YarnPiece(background)
    _ <- Cast.onDoubleBed(first, last, backgroundPiece)
    matrix = imageToPattern(img, width)
    _ <- FairIslePlanner.doubleBed(matrix, tension, Some(first))
    //TODO move all yarn to single bed
    _ <- Basics.knitRowWithK(settings = KCarriage.Settings(tension = tension),
      assembly = KCarriage.DoubleBedCarriage(tension = tension), yarnA = Some(backgroundPiece))
    _ <- Cast.offClosed(MainBed, backgroundPiece)
  } yield ()

  /**
   * Width/Height: Size of the laptop
   * Gauge: 10cm/10cm stitches (columns, rows)
   */
  def laptopHuelle(widthCm: Double, heightCm: Double, thicknessCm: Double, gauge: (Double, Double), yarnA: Yarn, yarnB: Yarn, xOffset: Int = 0): Planner = for {
    _ <- Planner.precondidtions(_ => true)
    thickness = (thicknessCm / 10 * gauge._1).round.toInt //per side
    border = 4 // per side

    width = (widthCm / 10 * gauge._1).round.toInt
    totalWidth = if (width + thickness % 2 != 0) width + thickness + 1 else width + thickness
    patternWidth = totalWidth - (border * 2)

    height = (heightCm / 10 * gauge._2).round.toInt
    patternHeight = height - (border * 2) + thickness
    borderRow = IndexedSeq.fill(totalWidth)(yarnA)
    borderRows = IndexedSeq.fill(border)(borderRow)
    borderAtSide = IndexedSeq.fill(border)(yarnA)

    first <- Planner.precondidtions { _ =>
      require(totalWidth <= Needle.count - 1)
      Needle.middle - (totalWidth / 2)
    }
    last = first + totalWidth - 1
    yarnAPiece <- Cast.onClosed(MainBed, first, last, yarnA)

    patternFront = IndexedSeq.tabulate(patternHeight, patternWidth) { (c, r) =>
      if (c % 2 == r % 2) yarnA
      else yarnB
    }
    pf = borderRows ++ patternFront.map(r => borderAtSide ++ r ++ borderAtSide) ++ borderRows
    _ <- FairIslePlanner.singleBed(pf, Some(first))

    rauteImg = ImageIO.read(new File("pattern/muster_raute.png"))
    raute = imageToPattern(rauteImg, rauteImg.getWidth)
    rauteRow = raute.map(one => Stream.continually(one).flatten.drop((rauteImg.getWidth - xOffset).abs).take(patternWidth))
    patternBack = Stream.continually(rauteRow).flatten.take(patternHeight)
    pb = borderRows ++ patternBack.map(r => borderAtSide ++ r ++ borderAtSide) ++ borderRows
    _ <- FairIslePlanner.singleBed(pb, Some(first))

    _ <- Cast.offClosed(MainBed, yarnAPiece)
  } yield ()


  def laptopHuelleRandom(widthCm: Double, heightCm: Double, gapCm: Double, lashCm: Double, thicknessCm: Double, gauge: (Double, Double),
    yarnBG: Yarn, yarnFront: Yarn, yarnBack: Yarn, yarnLash: Yarn, seed: Long = 0): Planner = for {
    _ <- Planner.precondidtions(_ => true)
    thickness = (thicknessCm / 10 * gauge._1).round.toInt //per side
    border = 1 // per side

    width = (widthCm / 10 * gauge._1).round.toInt + thickness + 2 * border
    patternWidth = width - 2 * border
    height = (heightCm / 10 * gauge._2).round.toInt + thickness
    lashHeight = (lashCm / 10 * gauge._2).round.toInt
    frontHeight = height - 1 - (gapCm / 10 * gauge._2).round.toInt

    rnd = new Random(seed)
    borderAtSide = IndexedSeq.fill(border)(yarnBG)

    frontPattern = (0 until frontHeight).map(_.toDouble / height).map { pA =>
      IndexedSeq.tabulate(patternWidth)(_ => if (rnd.nextDouble() < pA) yarnFront else yarnBG)
    }.map(borderAtSide ++ _ ++ borderAtSide)

    backPattern = (0 until height).map(_.toDouble / height).map { pB =>
      IndexedSeq.tabulate(patternWidth)(_ => if (rnd.nextDouble() > pB) yarnBack else yarnBG)
    }.map(borderAtSide ++ _ ++ borderAtSide)

    lashPattern = IndexedSeq.tabulate(lashHeight - 1, patternWidth) { (c, r) =>
      if (c % 2 == r % 2) yarnBG
      else yarnLash
    }.map(borderAtSide ++ _ ++ borderAtSide) :+ IndexedSeq.fill(width)(yarnBG)


    first <- Planner.precondidtions { _ =>
      require(width <= Needle.count - 1)
      Needle.middle - (width / 2)
    }
    last = first + width - 1
    bg <- Cast.onClosed(MainBed, first, last, yarnBG)
    _ <- Basics.knitRowWithK(yarnA = Some(bg))
    _ <- FairIslePlanner.singleBed(frontPattern, Some(first))
    _ <- FairIslePlanner.singleBed(backPattern, Some(first))
    _ <- FairIslePlanner.singleBed(lashPattern, Some(first))
  } yield ()

  case class Dimension(width: Int, height: Int)
  type Pattern = Matrix[Yarn]

  //TODO implement


  case class Patterns(front: Pattern, back: Pattern, lash: Pattern)

  def laptopBase(width: Length, height: Length, topGap: Length, lash: Length, thickness: Length,
    gauge: Gauge, patterns: (Dimension, Dimension, Dimension) => Patterns): Planner = for {
    _ <- Planner.precondidtions(_ => true)
    border = 2 //stitches used to the parts sew together

    widthBody = gauge.stitchesFor(width + thickness) + border
    heightFront = gauge.rowsFor(height + thickness / 2 - topGap) - 1
    heightBack = gauge.rowsFor(height + thickness)
    lashWidth = widthBody - ((widthBody - gauge.stitchesFor(width)) / 2 * 2)
    lashHeight = gauge.rowsFor(lash)

    frontDim = Dimension(widthBody, heightFront)
    backDim = Dimension(widthBody, heightBack)
    lashDim = Dimension(lashWidth, lashHeight)

    ps <- Planner.precondidtions { _ =>
      val ps = patterns(frontDim, backDim, lashDim)
      require(ps.front.size == frontDim.height, "Wrong front height")
      require(ps.front.head.size == frontDim.width, "Wrong front width")
      require(ps.back.size == backDim.height, "Wrong back height")
      require(ps.back.head.size == backDim.width, "Wrong back width")
      require(ps.lash.size == lashDim.height, "Wrong lash height")
      require(ps.lash.head.size == lashDim.width, "Wrong lash width")
      ps
    }
    first <- Planner.precondidtions { _ =>
      require(widthBody <= Needle.count - 1)
      Needle.middle - (widthBody / 2)
    }
    last = first + widthBody - 1
    toDecrease = (widthBody - lashWidth) / 2
    firstLash = first + toDecrease


    bg <- Cast.onClosed(MainBed, first, last, ps.front(0)(0))
    _ <- Basics.knitRowWithK(yarnA = Some(bg))
    _ <- FairIslePlanner.singleBed(ps.front, Some(first))
    _ <- FairIslePlanner.singleBed(ps.back.reverse, Some(first))
    _ <- FairIslePlanner.singleBed(ps.lash.take(1), Some(firstLash))
    _ <- (1 to toDecrease).toVector.traverse { i =>
      FormGiving.raglanDecrease(MainBed, Left) >>
        FormGiving.raglanDecrease(MainBed, Right) >>
        FairIslePlanner.singleBed(ps.lash.drop(i).take(1), Some(firstLash))
    }
    _ <- FairIslePlanner.singleBed(ps.lash.drop(toDecrease + 1), Some(firstLash))
    _ <- Cast.offClosed(MainBed, bg)
  } yield ()

  def laptopRndCheckerboard(width: Length, height: Length, topGap: Length, lash: Length, thickness: Length,
    gauge: Gauge, yarnA: Yarn, yarnB: Yarn) = {
    val squareSize = 2.cm
    val squareWidth = gauge.stitchesFor(squareSize)
    val squareHeight = gauge.rowsFor(squareSize)

    def checkerboard(w: Int) = {
      val offset = (w % squareWidth) / 2
      val lineA = Stream.continually(Stream.fill(squareWidth)(yarnA) ++ Stream.fill(squareWidth)(yarnB)).flatten.drop(offset)
      val lineB = lineA.drop(squareWidth)
      val a = lineA.take(w).toIndexedSeq
      val b = lineB.take(w).toIndexedSeq
      Stream.continually(Stream.fill(squareHeight)(a) ++ Stream.fill(squareHeight)(b)).flatten
    }

    val rnd = new Random(0)
    def randomize(pattern: Matrix[Yarn], prob: Int => Double, yarn: Yarn) = {
      pattern.zipWithIndex.map {
        case (row, i) =>
          val p = prob(i)
          row.map { y =>
            if (rnd.nextDouble() < p) yarn
            else y
          }
      }
    }

    def rf(count: Int, offset: Int)(i: Int) = {
      val sector = (i + offset) / squareHeight
      val sectors = count / squareHeight
      Math.pow((sectors - sector - 1).toDouble / sectors, 1.5)
    }

    def pattern(frontDim: Dimension, backDim: Dimension, lashDim: Dimension) = {
      require(frontDim.width == backDim.width)

      val frontOffset = squareHeight - (frontDim.height % squareHeight)
      val front = randomize(checkerboard(frontDim.width).drop(frontOffset).take(frontDim.height).toIndexedSeq, rf(frontDim.height, frontOffset), yarnB)
      val backOffset = squareHeight - (backDim.height % squareHeight)
      val back = randomize(checkerboard(frontDim.width).drop(backOffset).take(backDim.height).toIndexedSeq, rf(backDim.height, backOffset), yarnA)
      val lash = IndexedSeq.fill(lashDim.height, lashDim.width)(yarnA)
      Patterns(front, back, lash)
    }

    laptopBase(width, height, topGap, lash, thickness, gauge, pattern)
  }


  def imageRag(img: BufferedImage, bg: Option[Yarn] = None) = {
    val w = img.getWidth.min(200)
    val pattern = imageToPattern(img, w)

    val yarn1 = YarnPiece(bg.getOrElse(pattern(0)(0)))
    val zero = 100 - w / 2

    Cast.onClosed(MainBed, Needle.atIndex(zero), Needle.atIndex(zero + w - 1), yarn1) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      FairIslePlanner.singleBed(pattern) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Cast.offClosed(MainBed, yarn1)
  }
  def imageRagDoubleBed(img: BufferedImage, tension: KCarriage.TensionDial = KCarriage.TensionDial.apply(0, 0), bg: Option[Yarn] = None) = {
    val w = img.getWidth.min(200)
    val pattern = imageToPattern(img, w)

    val yarn1 = YarnPiece(bg.getOrElse(pattern(0)(0)))
    val zero = 100 - w / 2
    val firstNeedle = Needle.atIndex(zero)
    val lastNeedle = Needle.atIndex(zero + w - 1)

    Cast.onClosed(MainBed, firstNeedle, lastNeedle, yarn1) >>
      Basics.moveNeedles(DoubleBed, n => n >= firstNeedle && n <= lastNeedle, NeedleB) >>
      Basics.knitRowWithK(yarnA = Some(yarn1), assembly = KCarriage.DoubleBedCarriage()) >>
      Basics.knitRowWithK(yarnA = Some(yarn1), assembly = KCarriage.DoubleBedCarriage()) >>
      FairIslePlanner.doubleBed(pattern, tension) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Cast.offClosed(MainBed, yarn1)
  }

  private def colorsToYarns(colors: Set[Color]) = {
    colors.zipWithIndex.map {
      case (c@Color.white, i) => c -> Yarn(s"White", new Color(0xf4f4f4))
      case (c@Color.black, i) => c -> Yarn(s"Black", c)
      case (c@Color.yellow, i) => c -> Yarn(s"Yellow", c)
      case (c@Color.red, i) => c -> Yarn(s"Red", c)
      case (c@Color.green, i) => c -> Yarn(s"Green", c)
      case (c@Color.blue, i) => c -> Yarn(s"Blue", c)
      case (color, i) => color -> Yarn(s"Yarn $i", color)
    }.toMap
  }

  private def imageToPattern(img: java.awt.image.BufferedImage, w: Int): IndexedSeq[IndexedSeq[models.Yarn]] = {
    val rgbs = (0 until img.getHeight).map { y =>
      (0 until w).map { x =>
        new Color(img.getRGB(x, y))
      }
    }
    val yarns = colorsToYarns(rgbs.flatten.toSet)
    val pattern = rgbs.matrixMap(yarns).reverseBoth
    pattern
  }

  def tube(width: Int, height: Int, yarn: YarnPiece) = {
    Cast.onClosedRound(Needle.middle - width / 2, Needle.middle + width / 2, yarn) >>
      (0 until height).toVector.traverse { _ =>
        Basics.knitRoundK(yarn)
      }
  }

  def decreasingTube(width: Int, height: Int, yarn: YarnPiece, every: Int = 4) = {
    Cast.onClosedRound(Needle.middle - width / 2, Needle.middle + width / 2, yarn) >>
      Basics.knitRoundK(yarn) >>
      Basics.knitRoundK(yarn) >>
      (1 to height / 2).toVector.traverse { i =>
        val decrease = if (i % every == 0) {
          FormGiving.raglanDecrease(MainBed, Right) >>
            FormGiving.raglanDecrease(MainBed, Left) >>
            FormGiving.raglanDecrease(DoubleBed, Right) >>
            FormGiving.raglanDecrease(DoubleBed, Left)
        } else Planner.noop
        decrease >>
          Basics.knitRoundK(yarn) >>
          Basics.knitRoundK(yarn)
      }
  }

  def sock(width: Int, shaftHeight: Int, footLength: Int, yarn: YarnPiece) = {
    import KCarriage.{Settings, HoldingCamH, DoubleBedCarriage}
    for {
      _ <- Cast.onClosedRound(Needle.middle - width / 2, Needle.middle + width / 2, yarn)
      _ <- (1 to shaftHeight).toVector.traverse { _ =>
        Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn)
      }
      heelSettings = Settings(holdingCamLever = HoldingCamH)
      heelAssembly = DoubleBedCarriage(partLeft = true, partRight = true)
      _ <- (1 to width / 3 * 2).toVector.traverse { _ =>
        reduceHeel >>
          Basics.knitRowWithK(heelSettings, heelAssembly, Some(yarn))
      }
      _ <- (1 to width / 3 * 2).toVector.traverse { _ =>
        extendHeel >>
          Basics.knitRowWithK(heelSettings, heelAssembly, Some(yarn))
      }
      _ <- (1 to footLength).toVector.traverse { _ =>
        Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn)
      }
      _ <- (1 to (width - 4) / 4).toVector.traverse { _ =>
        FormGiving.raglanDecrease(MainBed, Right) >>
          FormGiving.raglanDecrease(MainBed, Left) >>
          FormGiving.raglanDecrease(DoubleBed, Right) >>
          FormGiving.raglanDecrease(DoubleBed, Left) >>
          Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn) >>
          Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn)
      }
      _ <- (1 to (width - 4) / 4).toVector.traverse { _ =>
        FormGiving.raglanDecrease(MainBed, Right) >>
          FormGiving.raglanDecrease(MainBed, Left) >>
          FormGiving.raglanDecrease(DoubleBed, Right) >>
          FormGiving.raglanDecrease(DoubleBed, Left) >>
          Basics.knitRoundK(yarn) >> Basics.knitRoundK(yarn)
      }
    } yield ()
  }
  private def reduceHeel() = for {
    working <- Planner.state(s =>
      Needle.all.filter { n =>
        val pos = s.needles(MainBed)(n).position
        pos.isWorking && pos != NeedleE
      })
    dir <- Planner.validate(_.nextDirection(KCarriage))
    needle = if (dir == ToLeft) working.last else working.head
    needles <- Planner.state(_.needles(MainBed))
    _ <- Basics.moveNeedles(MainBed, _ == needle, NeedleE)
  } yield ()
  private def extendHeel() = for {
    working <- Planner.state(s =>
      Needle.all.filter { n =>
        val pos = s.needles(MainBed)(n).position
        pos.isWorking && pos != NeedleE
      })
    dir <- Planner.validate(_.nextDirection(KCarriage))
    needle = if (dir == ToLeft) working.last + 1 else working.head - 1
    needles <- Planner.state(_.needles(MainBed))
    _ <- Basics.moveNeedles(MainBed, _ == needle, NeedleD)
  } yield ()
}