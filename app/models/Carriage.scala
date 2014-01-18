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
    position: CarriagePosition = CarriageRemoved) extends CarriageState
  def initialState = State()

  sealed trait Assembly
  case class SinkerPlate(yarnA: Option[Yarn] = None, yarnB: Option[Yarn] = None,
    weavingPatternLeft: Boolean = false, weavingPatternRight: Boolean = false)
    extends Assembly

  case class Settings(mc: Boolean = false, l: Boolean = false,
    partLeft: Boolean = false, partRight: Boolean = false,
    tuckLeft: Boolean = false, tuckRight: Boolean = false,
    holdingCamLever: HoldingCamLever = HoldingCamN) {
    def knob = "KCII" //always use KC2 for assisted knitting
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
    position: CarriagePosition = CarriageRemoved) extends CarriageState
  def initialState = State()

  //TODO
  case class Settings()
}
