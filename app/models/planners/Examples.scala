package models.planners

import java.awt.Color
import java.awt.image.BufferedImage
import scalaz._
import Scalaz._
import models._
import utils._
import models.plan.Planner

object Examples {

  def imageRag(img: BufferedImage, bg: Option[Yarn] = None) = {
    val w = img.getWidth.min(200)
    val pattern = imageToPattern(img, w)

    val yarn1 = YarnPiece(bg.getOrElse(pattern(0)(0)))
    val zero = 100 - w / 2

    Cast.onClosed(Needle.atIndex(zero), Needle.atIndex(zero + w - 1), yarn1) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      FairIslePlanner.singleBed(pattern) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Cast.offClosed(yarn1)
  }
  def imageRagDoubleBed(img: BufferedImage, bg: Option[Yarn] = None) = {
    val w = img.getWidth.min(200)
    val pattern = imageToPattern(img, w)

    val yarn1 = YarnPiece(bg.getOrElse(pattern(0)(0)))
    val zero = 100 - w / 2

    Cast.onClosed(Needle.atIndex(zero), Needle.atIndex(zero + w - 1), yarn1) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      FairIslePlanner.doubleBed(pattern) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Basics.knitRowWithK(yarnA = Some(yarn1)) >>
      Cast.offClosed(yarn1)
  }

  private def colorsToYarns(colors: Set[Color]) = {
    colors.zipWithIndex.map {
      case (c @ Color.white, i) => (c -> Yarn(s"White", new Color(0xf4f4f4)))
      case (c @ Color.black, i) => (c -> Yarn(s"Black", c))
      case (color, i) => (color -> Yarn(s"Yarn $i", color))
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
    val planner =
      Cast.onClosedRound(Needle.middle - width / 2, Needle.middle + width / 2, yarn) >>
        (0 until height).toVector.traverse { _ =>
          Basics.knitRoundK(yarn)
        }
    planner.plan()
  }

  def decreasingTube(width: Int, height: Int, yarn: YarnPiece, every: Int = 2) = {
    val planner =
      Cast.onClosedRound(Needle.middle - width / 2, Needle.middle + width / 2, yarn) >>
        Basics.knitRoundK(yarn) >>
        Basics.knitRoundK(yarn) >>
        (1 to height / 2).toVector.traverse { i =>
          val decrease = if (i % every == 0) {
            FormGiving.raglanDecrease(Right) >>
              FormGiving.raglanDecrease(Left)
          } else Planner.noop
          decrease >>
            Basics.knitRoundK(yarn) >>
            Basics.knitRoundK(yarn)
        }
    planner.plan()
  }
}