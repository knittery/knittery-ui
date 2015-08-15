package knit.planners

import scalaz._
import Scalaz._
import knit._
import knit.plan._
import utils._
import KCarriage.{SlideLeverIiIi, KRChangeKnobIiIi}

/**
 * Knits a fair isle pattern (aka Norwegermuster).
 */
object FairIslePlanner {

  def doubleBed(pattern: Matrix[Yarn], tension: Tension, startNeedle: Option[Needle] = None) = for {
    workingNeedles <- Planner.state(_.workingNeedles)
    _ <- Planner.precondidtions(_ => require(workingNeedles.nonEmpty, "No working needles"))
    _ <- checkPattern(pattern)
    _ <- Planner.precondidtions(_ => require(pattern.width % 2 == 0, "Pattern with non-even width"))
    needle0 = startNeedle.getOrElse(workingNeedles.head)
    doubleBedNeedles = (n: Needle) => n >= needle0 && n < (needle0 + pattern.width)
    pattern2 <- patternToYarnPiece(pattern)
    _ <- Basics.needCarriage(KCarriage, Left)
    _ <- Basics.moveNeedles(DoubleBed, doubleBedNeedles, NeedleB)
    tensionDial = KCarriage.TensionDial(tension)
    _ <- Basics.knitRowWithK(settings = KCarriage.Settings(tension = tensionDial), yarnA = Some(pattern2.head.head),
      assembly = KCarriage.DoubleBedCarriage(knobLeft = KRChangeKnobIiIi, knobRight = KRChangeKnobIiIi, slideLever = SlideLeverIiIi,
        partLeft = true, partRight = true, tension = tensionDial))
    _ <- pattern2.rows.toVector.traverse(row => knitDoubleBedRow(row, needle0, tension))
  } yield ()

  private def knitDoubleBedRow(row: Seq[YarnPiece], startNeedle: Needle, tension: Tension) = for {
    yarnA <- Planner.state(_.carriageState(KCarriage).yarnA.getOrElse(row.head))
    yarnB = (row.toSet - yarnA).headOption
    tensionDial = KCarriage.TensionDial(tension)
    settings = KCarriage.Settings(tension = tensionDial, partLeft = true, partRight = true)
    dbSettings = KCarriage.DoubleBedCarriage(knobLeft = KRChangeKnobIiIi, knobRight = KRChangeKnobIiIi,
      tension = tensionDial, partLeft = true, partRight = true,
      slideLever = SlideLeverIiIi)
    //TODO background yarn...
    //with A
    _ <- Basics.knitRowWithK(settings, dbSettings, Some(yarnA), None,
      knitActionDoubleBed(row, startNeedle, Some(yarnA), knitBackground = false))
    //with B
    _ <- Basics.knitRowWithK(settings, dbSettings, yarnB.orElse(Some(yarnA)), None,
      knitActionDoubleBed(row, startNeedle, yarnB, knitBackground = false))
  } yield ()

  private def knitActionDoubleBed(row: Seq[YarnPiece], startNeedle: Needle, yarn: Option[YarnPiece], knitBackground: Boolean)(needle: Needle) = {
    val index = needle.index - startNeedle.index
    if (yarn.isEmpty) NeedleToB
    else if (index < 0 || index >= row.size) if (knitBackground) NeedleToD else NeedleToB
    else if (row(index) == yarn.get) NeedleToD
    else NeedleToB
  }

  /**
   * Knit the pattern as a single bed fair isle pattern (row 0 in matrix is knitted first).
   * Does not change the working position of the needles, at least one needle (or better all for the
   * pattern) must be in working position.
   */
  def singleBed(pattern: Matrix[Yarn], startNeedle: Option[Needle] = None) = for {
    workingNeedles <- Planner.state(_.workingNeedles)
    _ <- Planner.precondidtions(_ => require(workingNeedles.nonEmpty, "No working needles"))
    _ <- checkPattern(pattern)
    needle0 = startNeedle.getOrElse(workingNeedles.head)
    settings = KCarriage.Settings(mc = true)
    _ <- Basics.needCarriage(KCarriage)
    pattern2 <- patternToYarnPiece(pattern)
    //Knit the pattern rows
    _ <- pattern2.rows.toVector.traverse(row =>
      for {
        yarns <- optimizeYarn(row.toSet)
        actionRow = knitActions(row, needle0, yarns) _
        _ <- Basics.knitRowWithK(settings, KCarriage.SinkerPlate(), yarns._1, yarns._2, actionRow)
      } yield ())
  } yield ()

  private def checkPattern(pattern: Matrix[Yarn]) = Planner.precondidtions { _ =>
    pattern.validate()
    require(pattern.height > 0, "Empty pattern")
    pattern.rows.map(_.toSet).zipWithIndex.foreach {
      case (yarns, index) =>
        require(yarns.size <= 2,
          s"fair isle pattern only support max two yarns per row. Row $index uses ${yarns.map(y => s"$y (${y.color})").mkString(", ")}")
    }
  }

  private def patternToYarnPiece(pattern: Matrix[Yarn]): PlannerM[Matrix[YarnPiece]] = for {
    pieces <- pattern.flatten.toSet.toVector.traverse { yarn =>
      Basics.nearestYarn(yarn).map(_.getOrElse(YarnPiece(yarn))).
        map(f => (yarn, f.start))
    }
    map = pieces.toMap
  } yield pattern.matrixMap(map)

  //TODO also take into account the next rows to knit => make it a general optimization?
  private def optimizeYarn(required: Set[YarnPiece]) = {
    for {
      yarnA <- Planner.state(_.carriageState(KCarriage).yarnA)
      yarnB <- Planner.state(_.carriageState(KCarriage).yarnB)
      available = yarnA.toSet ++ yarnB.toSet
    } yield {
      if (required.forall(available.contains)) (yarnA, yarnB)
      else required.toList match {
        case one :: two :: Nil => (Some(one), Some(two))
        case one :: Nil => (Some(one), None)
        case other => throw new IllegalStateException(s"Invalid yarn configuration: $other")
      }
    }
  }

  private def knitActions(row: Seq[YarnPiece], startNeedle: Needle, yarns: (Option[YarnFlow], Option[YarnFlow]))(needle: Needle) = {
    val index = needle.index - startNeedle.index
    if (index < 0 || index >= row.size) NeedleToB
    else {
      val yarnA = yarns._1.map(_.start)
      val yarnB = yarns._2.map(_.start)
      Some(row(index)) match {
        case `yarnA` => NeedleToB
        case `yarnB` => NeedleToD
        case Some(x) => throw new IllegalStateException(s"want to use yarn ${x.yarn} but that's not on the carriage")
      }
    }
  }
}
