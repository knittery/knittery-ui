package models.planners

import java.awt.Color
import java.awt.image.BufferedImage
import scalaz._
import Scalaz._
import models._
import utils._

object Examples {

  def imageRag(img: BufferedImage, bg: Option[Yarn] = None) = {
    val w = img.getWidth.min(200)
    val rgbs = (0 until img.getHeight).map { y =>
      (0 until w).map { x =>
        new Color(img.getRGB(x, y))
      }
    }
    val yarns = colorsToYarns(rgbs.flatten.toSet)
    val pattern = rgbs.matrixMap(yarns).reverseBoth

    val yarn1 = YarnStart(bg.getOrElse(pattern(0)(0)))
    val zero = 100 - w / 2

    Cast.onClosed(Needle.atIndex(zero), Needle.atIndex(zero + w - 1), yarn1) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      FairIslePlanner.singleBed(pattern) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      Basics.knitRowWithK(KCarriage.Settings(), Some(yarn1)) >>
      Cast.offClosed(yarn1)
  }

  private def colorsToYarns(colors: Set[Color]) = {
    colors.zipWithIndex.map {
      case (c @ Color.white, i) => (c -> Yarn(s"White", new Color(0xf4f4f4)))
      case (c @ Color.black, i) => (c -> Yarn(s"Black", c))
      case (color, i) => (color -> Yarn(s"Yarn $i", color))
    }.toMap
  }

}