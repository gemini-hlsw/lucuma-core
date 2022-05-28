// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all._
import lucuma.core.math.Angle
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

/**
  * Position Angle model
  */
sealed trait PosAngle extends Product with Serializable

object PosAngle {
  case class Fixed(angle: Angle) extends PosAngle {
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
  def fixed(angle: Angle): PosAngle =
    Fixed(angle)

  case class AllowFlip(angle: Angle) extends PosAngle {
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
  def allowFlip(angle: Angle): PosAngle =
    AllowFlip(angle)

  case object AverageParallactic extends PosAngle {
    override def toString: String = "AverageParallactic"
  }

  /**
   * Constructs an `AverageParallactic` instance with a wider `PosAngle` type.
   *
   *  @group Constructors
   */
  val averageParallactic: PosAngle =
    AverageParallactic

  case class ParallacticOverride(angle: Angle) extends PosAngle {
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
  def parallacticOverride(angle: Angle): PosAngle =
    ParallacticOverride(angle)

  case object Unconstrained extends PosAngle {
    override def toString: String = "Unconstrained"
  }

  /**
   *  Constructs an `Unconstrained` instance with a wider `PosAngle` type.
   *
   *  @group Constructors
   */
  val unconstrained: PosAngle =
    Unconstrained

  val Default: PosAngle = Fixed(Angle.Angle0)

  implicit val eqPosAngle: Eq[PosAngle] = Eq.instance {
    case (Fixed(a), Fixed(b))                             => a === b
    case (AllowFlip(a), AllowFlip(b))                     => a === b
    case (AverageParallactic, AverageParallactic)         => true
    case (ParallacticOverride(a), ParallacticOverride(b)) => a === b
    case (Unconstrained, Unconstrained)                   => true
    case _                                                => false
  }

  val fixedPrism: Prism[PosAngle, Fixed] = GenPrism[PosAngle, Fixed]

  val fixedAnglePrism: Optional[PosAngle, Angle] = fixedPrism.andThen(Fixed.angle)

  val allowFlipPrism: Prism[PosAngle, AllowFlip] = GenPrism[PosAngle, AllowFlip]

  val allowFlipAnglePrism: Optional[PosAngle, Angle] = allowFlipPrism.andThen(AllowFlip.angle)

  val parallacticOverridePrism: Prism[PosAngle, ParallacticOverride] =
    GenPrism[PosAngle, ParallacticOverride]

  val parallacticOverrideAnglePrism: Optional[PosAngle, Angle] =
    parallacticOverridePrism.andThen(ParallacticOverride.angle)

}
