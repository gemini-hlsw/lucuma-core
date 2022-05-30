// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all.*
import lucuma.core.math.Angle
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

/**
  * Position Angle Constraint model.  Defines how the position angle will be
 * constrained.
  */
sealed trait PosAngleConstraint extends Product with Serializable

object PosAngleConstraint {

  /**
   * Specifies that the position angle must be set to the specified angle.
   */
  case class Fixed(angle: Angle) extends PosAngleConstraint {
    override def toString: String = s"Fixed(${angle.toDoubleDegrees})"
  }

  object Fixed {
    val angle: Lens[Fixed, Angle] = Focus[Fixed](_.angle)
  }

  /**
   *  Constructs a `Fixed` instance with a wider `PosAngle` type.
   *
   *  @group Constructors
   */
  def fixed(angle: Angle): PosAngleConstraint =
    Fixed(angle)

  /**
   * Specifies a position angle equal to the given angle or the given angle
   * plus 180 degrees.
   */
  case class AllowFlip(angle: Angle) extends PosAngleConstraint {
    override def toString: String = s"AllowFlip(${angle.toDoubleDegrees})"
  }

  object AllowFlip {
    val angle: Lens[AllowFlip, Angle] = Focus[AllowFlip](_.angle)
  }

  /**
   *  Constructs an `AllowFlip` instance with a wider `PosAngle` type.
   *
   *  @group Constructors
   */
  def allowFlip(angle: Angle): PosAngleConstraint =
    AllowFlip(angle)

  /**
   * Specifies that the position angle be set to the average parallactic
   * angle at the time of the observation.
   */
  case object AverageParallactic extends PosAngleConstraint {
    override def toString: String = "AverageParallactic"
  }

  /**
   * Constructs an `AverageParallactic` instance with a wider `PosAngle` type.
   *
   *  @group Constructors
   */
  val averageParallactic: PosAngleConstraint =
    AverageParallactic

  /**
   * Specifies a fixed position angle, remembering that originally the
   * average parallactic angle was desired.
   */
  case class ParallacticOverride(angle: Angle) extends PosAngleConstraint {
    override def toString: String = s"ParallacticOverride(${angle.toDoubleDegrees})"
  }

  object ParallacticOverride {
    val angle: Lens[ParallacticOverride, Angle] = Focus[ParallacticOverride](_.angle)
  }

  /**
   * Constructs a `ParallacticOverride` instance with a wider `PosAngle` type.
   *
   *  @group Constructors
   */
  def parallacticOverride(angle: Angle): PosAngleConstraint =
    ParallacticOverride(angle)

  /**
   * Specifies that there is no constraint on the position angle.  It may be
   * set to any value for this observation.
   */
  case object Unconstrained extends PosAngleConstraint {
    override def toString: String = "Unconstrained"
  }

  /**
   *  Constructs an `Unconstrained` instance with a wider `PosAngle` type.
   *
   *  @group Constructors
   */
  val unconstrained: PosAngleConstraint =
    Unconstrained

  val Default: PosAngleConstraint = Fixed(Angle.Angle0)

  implicit val eqPosAngle: Eq[PosAngleConstraint] = Eq.instance {
    case (Fixed(a), Fixed(b))                             => a === b
    case (AllowFlip(a), AllowFlip(b))                     => a === b
    case (AverageParallactic, AverageParallactic)         => true
    case (ParallacticOverride(a), ParallacticOverride(b)) => a === b
    case (Unconstrained, Unconstrained)                   => true
    case _                                                => false
  }

  val fixedPrism: Prism[PosAngleConstraint, Fixed] = GenPrism[PosAngleConstraint, Fixed]

  val fixedAnglePrism: Optional[PosAngleConstraint, Angle] = fixedPrism.andThen(Fixed.angle)

  val allowFlipPrism: Prism[PosAngleConstraint, AllowFlip] = GenPrism[PosAngleConstraint, AllowFlip]

  val allowFlipAnglePrism: Optional[PosAngleConstraint, Angle] = allowFlipPrism.andThen(AllowFlip.angle)

  val parallacticOverridePrism: Prism[PosAngleConstraint, ParallacticOverride] =
    GenPrism[PosAngleConstraint, ParallacticOverride]

  val parallacticOverrideAnglePrism: Optional[PosAngleConstraint, Angle] =
    parallacticOverridePrism.andThen(ParallacticOverride.angle)

  val angleOptional: Optional[PosAngleConstraint, Angle] =
    Optional[PosAngleConstraint, Angle]({
      case Fixed(angle)               => angle.some
      case AllowFlip(angle)           => angle.some
      case AverageParallactic         => none
      case ParallacticOverride(angle) => angle.some
      case Unconstrained              => none
    })({ a => {
      case Fixed(_)                   => Fixed(a)
      case AllowFlip(_)               => AllowFlip(a)
      case AverageParallactic         => AverageParallactic
      case ParallacticOverride(_)     => ParallacticOverride(a)
      case Unconstrained              => Unconstrained
    }})

}
