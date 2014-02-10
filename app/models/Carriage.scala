package models
sealed trait Carriage {
  def name: String
  /** Needles the carriage is over if at the given position. */
  def over(pos: CarriagePosition): Seq[Needle] = pos match {
    case CarriageLeft(overlapCount) => Needle.all.take(overlapCount)
    case CarriageRight(overlapCount) => Needle.all.reverse.take(overlapCount)
    case CarriageOverNeedles(needle) => Needle.all.span(_ == needle) match {
      case (a, b) => a.reverse.take(width / 2).reverse ++ b.take(width / 2)
    }
    case CarriageRemoved => Seq.empty
  }
  /** Width of the carriage in needles. */
  protected def width: Int

  type State <: CarriageState
  def initialState: State
}

sealed trait CarriageState {
  def position: CarriagePosition
}

/** Normal carriage. */
case object KCarriage extends Carriage {
  override def name = "K"
  override protected def width = 66

  case class State(assembly: Assembly = KCarriage.SinkerPlate(),
    settings: Settings = Settings(),
    position: CarriagePosition = CarriageRemoved) extends CarriageState {
    def yarnA = yarns._1
    def yarnB = yarns._2
    def yarns = assembly match {
      case SinkerPlate(yarnA, yarnB, _, _) => (yarnA, yarnB)
      case c: DoubleBedCarriage => (c.yarn, None)
    }
  }
  def initialState = State()

  sealed trait Assembly
  case class SinkerPlate(yarnA: Option[YarnPiece] = None, yarnB: Option[YarnPiece] = None,
    weavingPatternLeft: Boolean = false, weavingPatternRight: Boolean = false)
    extends Assembly
  case class DoubleBedCarriage(yarn: Option[YarnPiece] = None,
    partLeft: Boolean = false, partRight: Boolean = false,
    needleTakebackLeft: Boolean = false, needleTakebackRight: Boolean = false)
    extends Assembly {
    def part(direction: Direction) = if (direction == ToLeft) partLeft else partRight
    def needleTakeback(direction: Direction) = if (direction == ToLeft) needleTakebackLeft else needleTakebackRight
  }

  case class Settings(mc: Boolean = false, l: Boolean = false,
    partLeft: Boolean = false, partRight: Boolean = false,
    tuckLeft: Boolean = false, tuckRight: Boolean = false,
    holdingCamLever: HoldingCamLever = HoldingCamN) {
    def knob = "KCII" //always use KC2 for assisted knitting
    def part(direction: Direction) = if (direction == ToLeft) partLeft else partRight
    def tuck(direction: Direction) = if (direction == ToLeft) tuckLeft else tuckRight
  }
  sealed trait HoldingCamLever {
    def name: String
  }
  case object HoldingCamN extends HoldingCamLever {
    override def name = "N"
  }
  case object HoldingCamH extends HoldingCamLever {
    override def name = "H"
  }
  case object HoldingCamI extends HoldingCamLever {
    override def name = "I"
  }
}

/** Lace pattern carriage. */
case object LCarriage extends Carriage {
  override def name = "L"
  override protected def width = 46

  case class State(
    settings: Settings = Settings(),
    position: CarriagePosition = CarriageRemoved) extends CarriageState
  def initialState = State()

  //TODO
  case class Settings()
}

/** Electronic carriage. */
case object GCarriage extends Carriage {
  override def name = "G"
  override protected def width = 20

  case class State(
    settings: Settings = Settings(),
    yarn: Option[YarnPiece] = None,
    position: CarriagePosition = CarriageRemoved) extends CarriageState
  def initialState = State()

  //TODO
  case class Settings()
}
