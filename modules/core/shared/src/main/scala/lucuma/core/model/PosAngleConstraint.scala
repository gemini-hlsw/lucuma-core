// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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

object PosAngleConstraint extends PosAngleConstraintOptics {

  /**
   * Specifies that the there is no constraint on the position angle.
   * It may be set to any angle for this observation.
   */
  case object Unbounded extends PosAngleConstraint {
    override def toString: String = "Unbounded"
  }

  /**
   * Constructs an `Unbounded` instance with a wider `PosAngle` type.
   *
   *  @group Constructors
   */
  val unbounded: PosAngleConstraint =
    Unbounded

  /**
   * Specifies that the position angle must be set to the specified angle.
   */
  final case class Fixed(angle: Angle) extends PosAngleConstraint {
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
  final case class AllowFlip(angle: Angle) extends PosAngleConstraint {
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
  final case class ParallacticOverride(angle: Angle) extends PosAngleConstraint {
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

  val Default: PosAngleConstraint = Fixed(Angle.Angle0)

  given Eq[PosAngleConstraint] = Eq.instance {
    case (Fixed(a), Fixed(b))                             => a === b
    case (AllowFlip(a), AllowFlip(b))                     => a === b
    case (AverageParallactic, AverageParallactic)         => true
    case (ParallacticOverride(a), ParallacticOverride(b)) => a === b
    case (Unbounded, Unbounded)                           => true
    case _                                                => false
  }

}

sealed trait PosAngleConstraintOptics { self: PosAngleConstraint.type =>

  /**
   * Optional that extracts the position angle for those constraints where a
   * particular angle is defined: `Fixed`, `AllowFlip` and `ParallacticOverride`.
   *
   * @group Optics
   */
  val angle: Optional[PosAngleConstraint, Angle] =
    Optional[PosAngleConstraint, Angle]({
      case PosAngleConstraint.Fixed(angle)               => angle.some
      case PosAngleConstraint.AllowFlip(angle)           => angle.some
      case PosAngleConstraint.AverageParallactic         => none
      case PosAngleConstraint.ParallacticOverride(angle) => angle.some
      case PosAngleConstraint.Unbounded                  => none
    })({ a => {
      case PosAngleConstraint.Fixed(_)                   => PosAngleConstraint.Fixed(a)
      case PosAngleConstraint.AllowFlip(_)               => PosAngleConstraint.AllowFlip(a)
      case PosAngleConstraint.AverageParallactic         => PosAngleConstraint.AverageParallactic
      case PosAngleConstraint.ParallacticOverride(_)     => PosAngleConstraint.ParallacticOverride(a)
      case PosAngleConstraint.Unbounded                  => PosAngleConstraint.Unbounded
    }})

  /**
   * @group Optics
   */
  val toFixed: Prism[PosAngleConstraint, PosAngleConstraint.Fixed] =
    GenPrism[PosAngleConstraint, PosAngleConstraint.Fixed]

  /**
   * @group Optics
   */
  val fixedAngle: Optional[PosAngleConstraint, Angle] =
    toFixed.andThen(PosAngleConstraint.Fixed.angle)

  /**
   * @group Optics
   */
  val toAllowFlip: Prism[PosAngleConstraint, PosAngleConstraint.AllowFlip] =
    GenPrism[PosAngleConstraint, PosAngleConstraint.AllowFlip]

  /**
   * @group Optics
   */
  val allowFlipAngle: Optional[PosAngleConstraint, Angle] =
    toAllowFlip.andThen(PosAngleConstraint.AllowFlip.angle)

  /**
   * @group Optics
   */
  val toParallacticOverride: Prism[PosAngleConstraint, PosAngleConstraint.ParallacticOverride] =
    GenPrism[PosAngleConstraint, PosAngleConstraint.ParallacticOverride]

  /**
   * @group Optics
   */
  val parallacticOverrideAngle: Optional[PosAngleConstraint, Angle] =
    toParallacticOverride.andThen(PosAngleConstraint.ParallacticOverride.angle)

}
