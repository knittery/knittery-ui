package models

import akka.actor._
import connector.Connector._

/**
 * Takes care of the current position in the pattern and updating the connector with the
 * current row.
 */
object NeedlePatternKnitter {
  def props(commander: ActorRef, connector: ActorRef) = {
    Props(new NeedlePatternKnitter(commander, connector))
  }

  import RowTracker.RowChanged
  case class SetNeedlePattern(pattern: NeedlePattern, startAtRow: Int = -1)
  case class NeedlePatternRowChanged(rowPattern: NeedlePatternRow, row: Int, pattern: NeedlePattern)

  private class NeedlePatternKnitter(commander: ActorRef, connector: ActorRef) extends Actor with ActorLogging {
    var row = -1
    var pattern = NeedlePattern.empty
    def patternRow = pattern.orElse(NeedlePattern.empty)(row) _

    override def preStart = {
      context watch connector
      updateRow
    }
    override def receive = {
      case SetNeedlePattern(p, r) =>
        pattern = p
        row = r
        log.info(s"New needle pattern loaded, $pattern starting at $row")
        commander ! NeedlePatternRowChanged(patternRow, row, pattern)
        updateRow
      case RowChanged(r) =>
        row = r
        commander ! NeedlePatternRowChanged(patternRow, row, pattern)
        updateRow
      case Terminated if sender == connector =>
        //connector crashed, reload current pattern row
        updateRow
    }

    def updateRow = {
      connector ! LoadPatternRow(patternRow.andThen(_ match {
        case NeedleD => NeedleToD
        case _ => NeedleToB
      }))
    }
  }
}
