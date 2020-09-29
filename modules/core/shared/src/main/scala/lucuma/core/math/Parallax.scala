// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import coulomb._
import coulomb.cats.implicits._
import lucuma.core.math.units._
import lucuma.core.optics._
import spire.math.Rational
import spire.std.long._

/**
 * Parallax stored as microarcseconds
 * Normally parallax is expressed in milliarcseconds but simbad reports them
 * with a higher precision thus using microarcseconds gives us enough space
 * Absolute allowed parallax values need to be less or equal thán 180 degrees
 */
sealed abstract case class Parallax protected (μas: Quantity[Long, MicroArcSecond]) {
  val mas: Quantity[Rational, MilliArcSecond] = μas.to[Rational, MilliArcSecond]

  override def toString: String =
    s"Parallax(${μas.show})"
}

object Parallax extends ParallaxOptics {
  val MaxValue: Parallax = apply(648000000000L)
  val MinValue: Parallax = apply(-648000000000L)

  /**
   * The `No parallax`
   * @group Constructors
   */
  val Zero: Parallax = apply(0L)

  // Internal unbounded constructor
  private def apply(μas: Long): Parallax =
    apply(μas.withUnit[MicroArcSecond])

  // Internal unbounded constructor
  private def apply(μas: Quantity[Long, MicroArcSecond]): Parallax =
    new Parallax(μas) {}

  /** @group Typeclass Instances */
  implicit val orderParallax: Order[Parallax] =
    Order.by(_.μas)

  /** @group Typeclass Instances */
  implicit val monoidParallax: Monoid[Parallax] =
    Monoid.instance(Zero, (a, b) => apply(a.μas + b.μas))

  /**
   * Construct a new Parallax of the given magnitude in integral microarcseconds, modulo 180°. Exact.
   * @group Constructors
   */
  def fromMicroarcseconds(μas: Long): Parallax = apply(Angle.signedMicroarcseconds.normalize(μas))

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
