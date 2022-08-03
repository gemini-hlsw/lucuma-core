// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import coulomb.*
import coulomb.ops.algebra.cats.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric._
import eu.timepit.refined.refineV
import lucuma.core.math.units.{*, given}
import lucuma.core.optics._
import spire.math.Rational
import spire.std.long._

/**
 * Parallax stored as microarcseconds
 * Normally parallax is expressed in milliarcseconds but simbad reports them
 * with a higher precision thus using microarcseconds gives us enough space.
 * Parallax values need to be in the interval [0°, 180°].
 * 180° is a theoretical limit - actual astronomical values will be very small.
 */
sealed abstract case class Parallax protected (
  μas: Quantity[Parallax.LongParallaxμas, MicroArcSecond]
) {
  val mas: Quantity[Rational, MilliArcSecond] = μas.toValue[Rational].toUnit[MilliArcSecond]

  override def toString: String =
    s"Parallax(${μas.show})"
}

object Parallax extends ParallaxOptics {
  type Parallaxμas     = Interval.Closed[0, Angle.Angle180µas]
  type LongParallaxμas = Long Refined Parallaxμas

  /**
   * The `No parallax`
   * @group Constructors
   */
  val Zero: Parallax = applyUnsafe(0L)

  // Internal unbounded constructor
  private def applyUnsafe(μas: Long): Parallax =
    apply(refineV[Parallaxμas](μas).toOption.get.withUnit[MicroArcSecond])

  def apply(μas: Quantity[LongParallaxμas, MicroArcSecond]): Parallax =
    new Parallax(μas) {}

  /** @group Typeclass Instances */
  implicit val orderParallax: Order[Parallax] =
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
      .imapB(_.underlying.movePointRight(3).longValue,
             n => new java.math.BigDecimal(n).movePointLeft(3)
      )

}
