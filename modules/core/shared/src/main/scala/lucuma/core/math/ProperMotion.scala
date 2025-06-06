// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import algebra.instances.all.given
import cats.*
import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import lucuma.core.math.units.*
import lucuma.core.optics.SplitMono
import lucuma.core.util.*
import monocle.Focus
import monocle.Iso
import monocle.Lens
import spire.math.Rational

object VelocityAxis {
  type RA
  type Dec
}

/**
  * ProperMotion contains Ra/Dec angular velocities
  */
case class ProperMotion( ra:  ProperMotion.RA, dec: ProperMotion.Dec) {
  // Return the ra/dec components in radians, first converting to degrees/y
  inline def toRadians: (Double, Double) =
    (ra.toRadians, dec.toRadians)

}

object ProperMotion extends ProperMotionOptics {
  type AngularVelocity = AngularVelocity.Type

  object AngularVelocity extends NewType[Quantity[Long, MicroArcSecondPerYear]] {

    private val Zero: AngularVelocity =
      AngularVelocity(0.withUnit[MicroArcSecondPerYear])

    def zeroOf[A]: AngularVelocity Of A = Zero.tag[A]

    /** @group Typeclass Instances */
    given Order[AngularVelocity] =
      Order.by(_.μasy.value)

    /** @group Typeclass Instances */
    def orderVelocityOf[A]: Order[AngularVelocity Of A] =
      Order.by(_.μasy.value)

    /** @group Typeclass Instances */
    given Monoid[AngularVelocity] =
      Monoid.instance(Zero, (a, b) => AngularVelocity(a.μasy + b.μasy))

    /** @group Typeclass Instances */
    def monoidVelocityOf[A]: Monoid[AngularVelocity Of A] =
      Monoid.instance(zeroOf[A], (a, b) => tag[A](AngularVelocity(a.μasy + b.μasy)))

    /** @group Typeclass Instances */
    given Show[AngularVelocity] =
      Show.fromToString
  }

  extension[A](self: AngularVelocity)
    inline def μasy: Quantity[Long, MicroArcSecondPerYear] = self.value

    // Direct conversion via coulomb turns to be too slow
    inline def toRadians: Double = (μasy.value.toDouble / (3600 * 1e6)).toRadians

    def masy: Quantity[Rational, MilliArcSecondPerYear] = μasy.toValue[Rational].toUnit[MilliArcSecondPerYear]

  sealed trait AngularVelocityOptics[A] {

    def μasy: Iso[Long, AngularVelocity Of A] =
      Iso[Long, AngularVelocity Of A](v =>
        tag[A](AngularVelocity(v.withUnit[MicroArcSecondPerYear]))
      )(_.μasy.value)

    def microarcsecondsPerYear: Iso[Long, AngularVelocity Of A] = μasy

    /**
      * This `AngularVelocity` as signed decimal milliseconds.
      */
    def milliarcsecondsPerYear: SplitMono[AngularVelocity Of A, BigDecimal] =
      SplitMono
        .fromIso[AngularVelocity Of A, Long](microarcsecondsPerYear.reverse)
        .imapB(_.underlying.movePointRight(3).longValue,
               n => new java.math.BigDecimal(n).movePointLeft(3)
        )
  }

  type RA  = AngularVelocity Of VelocityAxis.RA
  object RA extends AngularVelocityOptics[VelocityAxis.RA]
  type Dec = AngularVelocity Of VelocityAxis.Dec
  object Dec extends AngularVelocityOptics[VelocityAxis.Dec]

  val ZeroRAVelocity: RA = AngularVelocity.zeroOf[VelocityAxis.RA]
  val ZeroDecVelocity: Dec = AngularVelocity.zeroOf[VelocityAxis.Dec]

  private [math] def μasyVelocity[A](μasy: Long): AngularVelocity Of A =
    AngularVelocity(μasy.withUnit[MicroArcSecondPerYear]).tag[A]
  def μasyRA(μasy: Long): AngularVelocity Of VelocityAxis.RA =
    μasyVelocity[VelocityAxis.RA](μasy)
  def μasyDec(μasy: Long): AngularVelocity Of VelocityAxis.Dec =
    μasyVelocity[VelocityAxis.Dec](μasy)

  given Order[RA] = AngularVelocity.orderVelocityOf[VelocityAxis.RA]
  given Monoid[RA] = AngularVelocity.monoidVelocityOf[VelocityAxis.RA]
  given Order[Dec] = AngularVelocity.orderVelocityOf[VelocityAxis.Dec]
  given Monoid[Dec] = AngularVelocity.monoidVelocityOf[VelocityAxis.Dec]

  /**
    * The `No parallax`
    * @group Constructors
    */
  val Zero: ProperMotion =
    ProperMotion(ZeroRAVelocity, ZeroDecVelocity)

  /** @group Typeclass Instances */
  given Order[ProperMotion] =
    Order.by(x => (x.ra, x.dec))

  /** @group Typeclass Instances */
  given Monoid[ProperMotion] =
    Monoid.instance(Zero, (a, b) => ProperMotion(a.ra |+| b.ra, a.dec |+| b.dec))

}

sealed trait ProperMotionOptics {

  /** @group Optics */
  val ra: Lens[ProperMotion, ProperMotion.RA] =
    Focus[ProperMotion](_.ra)

  /** @group Optics */
  val dec: Lens[ProperMotion, ProperMotion.Dec] =
    Focus[ProperMotion](_.dec)

  private def splitMonoFromComponents[A](
    raMono:  SplitMono[ProperMotion.RA, A],
    decMono: SplitMono[ProperMotion.Dec, A]
  ): SplitMono[ProperMotion, (A, A)] =
    SplitMono(
      v => (raMono.get(v.ra), decMono.get(v.dec)),
      t => ProperMotion(raMono.reverseGet(t._1), decMono.reverseGet(t._2))
    )

  /** @group Optics */
  val milliarcsecondsPerYear: SplitMono[ProperMotion, (BigDecimal, BigDecimal)] =
    splitMonoFromComponents(
      ProperMotion.RA.milliarcsecondsPerYear,
      ProperMotion.Dec.milliarcsecondsPerYear
    )

}
