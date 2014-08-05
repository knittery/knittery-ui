package models.planners

import java.awt.Color
import java.awt.image.BufferedImage
import scalaz._
import Scalaz._
import models._
import utils._
import models.plan._
import models.KCarriage.TensionDial
import javax.imageio.ImageIO
import java.io.File

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