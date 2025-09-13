// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.*
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.api.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.refineV
import lucuma.core.math.units.{*, given}
import lucuma.core.optics.*
import spire.math.Rational
import spire.std.long.*
import lucuma.core.refined.given
import coulomb.integrations.cats.quantity.given

import scala.math.BigDecimal.RoundingMode

/**
 * Parallax stored as microarcseconds
 * Normally parallax is expressed in milliarcseconds but simbad reports them
 * with a higher precision thus using microarcseconds gives us enough space.
 * Parallax values need to be in the interval [0°, 180°].
 * 180° is a theoretical limit - actual astronomical values will be very small.
 */
opaque type Parallax = Quantity[Parallax.LongParallaxμas, MicroArcSecond]

object Parallax extends ParallaxOptics {
  type Parallaxμas     = Interval.Closed[0, Angle.Angle180µas]
  type LongParallaxμas = Long Refined Parallaxμas

  extension(px: Parallax)
    def μas: Quantity[Parallax.LongParallaxμas, MicroArcSecond] = px

    def mas: Quantity[Rational, MilliArcSecond] = μas.toValue[Rational].toUnit[MilliArcSecond]

  /**
   * The `No parallax`
   * @group Constructors
   */
  val Zero: Parallax = applyUnsafe(0L)

  // Internal unbounded constructor
  private def applyUnsafe(μas: Long): Parallax =
    apply(refineV[Parallaxμas](μas).toOption.get)

  def apply(μas: LongParallaxμas): Parallax =
    μas.withUnit[MicroArcSecond]

  /** @group Typeclass Instances */
  given Order[Parallax] =
    Order.by(_.μas)

  /**
   * Construct a new Parallax of the given magnitude in integral microarcseconds. Exact.
   * Finds the normalized signed angle for the microarcseconds, then takes
   * the absolute value.
   * @group Constructors
   */
  def fromMicroarcseconds(μas: Long): Parallax =
    applyUnsafe(Angle.signedMicroarcseconds.normalize(μas).abs)
}

sealed trait ParallaxOptics {

  /**
   * This `Parallax` in microarcseconds. modulo 180°.
   */
  lazy val microarcseconds: SplitMono[Parallax, Long] =
    SplitMono(_.μas.value, Parallax.fromMicroarcseconds)

  lazy val μas: SplitMono[Parallax, Long] = microarcseconds

  /**
   * This `Parallax` as in milliarcseconds.
   */
  lazy val milliarcseconds: SplitMono[Parallax, BigDecimal] =
    microarcseconds
      .imapB(
        _.setScale(3, RoundingMode.HALF_UP).underlying.movePointRight(3).longValue,
        n => new java.math.BigDecimal(n).movePointLeft(3)
      )

}
