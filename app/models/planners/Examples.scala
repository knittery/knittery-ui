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
import models.units._
import models._
import models.plan._
import models.KCarriage.TensionDial

object Examples {

  def handyHuelle(img: BufferedImage, background: Yarn, tension: Tension): Planner = for {
    width <- Planner.precondidtions(_ => img.getWidth)
    height <- Planner.precondidtions(_ => img.getHeight)
    first <- Planner.precondidtions { _ =>
      require(width <= Needle.count - 1)
      Needle.middle - (width / 2)
    }
    last = first + width
    backgroundPiece = YarnPiece(background)
    _ <- Cast.onDoubleBed(first, last, backgroundPiece)
    matrix = Helper.imageToPattern(img).takeWidth(width)
    _ <- FairIslePlanner.doubleBed(matrix, tension, Some(first))
    //TODO move all yarn to single bed
    tensionDial = KCarriage.TensionDial(tension)
    _ <- Basics.knitRowWithK(settings = KCarriage.Settings(tension = tensionDial),
      assembly = KCarriage.DoubleBedCarriage(tension = tensionDial), yarnA = Some(backgroundPiece))
    _ <- Cast.offClosed(MainBed, backgroundPiece)
  } yield ()

  def imageRag(img: BufferedImage, bg: Option[Yarn] = None) = {
    val w = img.getWidth.min(200)
    val pattern = Helper.imageToPattern(img).takeWidth(w)

    val yarn1 = YarnPiece(bg.getOrElse(pattern(0)(0)))
    val zero = 100 - w / 2

    Cast.onClosed(MainBed, Needle.atIndex(zero), Needle.atIndex(zero + w - 1), yarn1) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      FairIslePlanner.singleBed(pattern) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Cast.offClosed(MainBed, yarn1)
  }
  def imageRagDoubleBed(img: BufferedImage, tension: Tension, bg: Option[Yarn] = None) = {
    val w = img.getWidth.min(200)
    val pattern = Helper.imageToPattern(img).takeWidth(w)

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


}