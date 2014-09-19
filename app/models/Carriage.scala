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
  def width: Int

  type State <: CarriageState
  def initialState: State
}

sealed trait CarriageState {
  def position: CarriagePosition
}

/** Normal carriage. */
case object KCarriage extends Carriage {
  override def name = "K"
  override def width = 66

  case class State(assembly: Assembly = KCarriage.SinkerPlate(),
    settings: Settings = Settings(),
    yarnA: Option[YarnPiece] = None,
    yarnB: Option[YarnPiece] = None,
    position: CarriagePosition = CarriageRemoved) extends CarriageState {
    require(yarnB.isEmpty || assembly.supportsYarnB, "No YarnB supported with this assembly")
    def yarns = (yarnA, yarnB)
  }
  def initialState = State()

  sealed trait Assembly {
    def supportsYarnB = false
  }
  case class SinkerPlate(weavingPatternLeft: Boolean = false, weavingPatternRight: Boolean = false)
    extends Assembly {
    override def supportsYarnB = true
  }
  case class DoubleBedCarriage(tension: TensionDial = TensionDial.zero,
    slideLever: SlideLever = SlideLeverII,
    knobLeft: KRChangeKnob = KRChangeKnobPlain, knobRight: KRChangeKnob = KRChangeKnobPlain,
    tuckingLever: TuckingLever = TuckingLeverR,
    partLeft: Boolean = false, partRight: Boolean = false,
    needleTakebackLeft: Boolean = true, needleTakebackRight: Boolean = true)
    extends Assembly {
    def part(direction: Direction) = if (direction == ToLeft) partLeft else partRight
    def needleTakeback(direction: Direction) = if (direction == ToLeft) needleTakebackLeft else needleTakebackRight
    def knob(direction: Direction) = if (direction == ToLeft) knobLeft else knobRight
  }

  case class Settings(tension: TensionDial = TensionDial.zero,
    mc: Boolean = false, l: Boolean = false,
    partLeft: Boolean = false, partRight: Boolean = false,
    tuckLeft: Boolean = false, tuckRight: Boolean = false,
    holdingCamLever: HoldingCamLever = HoldingCamN) {
    def knob = "KCII"
    //always use KC2 for assisted knitting
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

  class TensionDial private(val number: Int, val thirds: Int) {
    def tension: Tension = Tension(number + thirds.toDouble / 3)
    /** Format: 5 1/3 */
    def text: String = number.toString + (if (thirds != 0) s"$thirds/3" else "")
    override def toString = text
  }
  object TensionDial {
    def apply(number: Int, thirds: Int) = {
      require(thirds >= 0 && thirds <= 3, s"Thirds must be [0,3] but is $thirds")
      val value = new TensionDial(number, thirds)
      require(value.tension.value > -0.01 && value.tension.value < 10.01, s"Only tensions between 0 and 10 are valid")
      value
    }
    def apply(tension: Tension): TensionDial = {
      val thirds = ((tension.value % 1) * 3 / 3).round.toInt
      val whole = tension.value.toInt
      if (whole.toDouble + (thirds.toDouble / 3) - tension.value < -0.5) apply(whole + 1, thirds)
      else apply(whole, thirds)
    }
    val zero = apply(0, 0)
  }

  sealed trait KRChangeKnob
  /** Every second needle is selected. */
  case object KRChangeKnobIiIi extends KRChangeKnob
  /** Knit with all needles. */
  case object KRChangeKnobPlain extends KRChangeKnob

  sealed trait SlideLever
  /** Knit with softer and looser stitch. */
  case object SlideLeverI extends SlideLever
  /** Knit with firmer stitch. */
  case object SlideLeverII extends SlideLever
  /** Use with KRChangeKnobIiIi. */
  case object SlideLeverIiIi extends SlideLever

  sealed trait TuckingLever
  /** Normal knitting. */
  case object TuckingLeverR extends TuckingLever
  /** English rib or racking patterns. */
  case object TuckingLeverP extends TuckingLever
}

/** Lace pattern carriage. */
case object LCarriage extends Carriage {
  override def name = "L"
  override def width = 46

  case class State(
    settings: Settings = Settings(),
    position: CarriagePosition = CarriageRemoved) extends CarriageState
  def initialState = State()

  case class Settings(mode: Mode = Lace)

  sealed trait Mode
  case object FineLace extends Mode
  case object Lace extends Mode
}

/** Electronic carriage. */
case object GCarriage extends Carriage {
  override def name = "G"
  override def width = 20

  case class State(
    settings: Settings = Settings(),
    yarn: Option[YarnPiece] = None,
    position: CarriagePosition = CarriageRemoved) extends CarriageState
  def initialState = State()

  //TODO
  case class Settings()
}
