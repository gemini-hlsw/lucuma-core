// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import cats.syntax.all._
import coulomb._
import coulomb.accepted._
import coulomb.cats.implicits._
import lucuma.core.math.ProperVelocity.AngularVelocityComponent
import lucuma.core.math.units._
import lucuma.core.optics.SplitMono
import monocle.Iso
import monocle.Lens
import monocle.macros.GenLens
import spire.math.Rational

object VelocityAxis {
  type RA
  type Dec
}

/**
  * ProperVelocity contains Ra/Dec angular velocities
  */
final case class ProperVelocity(
  ra:  ProperVelocity.AngularVelocityComponent[VelocityAxis.RA],
  dec: ProperVelocity.AngularVelocityComponent[VelocityAxis.Dec]
) {
  // Return the ra/dec components in radians, first converting to degrees/y
  def toRadians: (Double, Double) =
    (ra.toRadians, dec.toRadians)

}

object ProperVelocity extends ProperVelocityOptics {
  final case class AngularVelocityComponent[A](μasy: Quantity[Long, MicroArcSecondPerYear]) {
    def toRadians: Double = μasy.to[Double, Degree %/ Year].value.toRadians

    val masy: Quantity[Rational, MilliArcSecondPerYear] = μasy.to[Rational, MilliArcSecondPerYear]

    override def toString =
      s"AngularVelocityComponent(${masy.show})"
  }

  object AngularVelocityComponent extends AngularVelocityComponentOptics {
    def Zero[A]: AngularVelocityComponent[A] =
      AngularVelocityComponent(0.withUnit[MicroArcSecondPerYear])

    /** @group Typeclass Instances */
    implicit def orderAngularVelocity[A]: Order[AngularVelocityComponent[A]] =
      Order.by(_.μasy)

    /** @group Typeclass Instances */
    implicit def monoidAngularVelocity[A]: Monoid[AngularVelocityComponent[A]] =
      Monoid.instance(Zero[A], (a, b) => AngularVelocityComponent[A](a.μasy |+| b.μasy))

    /** @group Typeclass Instances */
    implicit def showAngularVelocity[A]: Show[AngularVelocityComponent[A]] =
      Show.fromToString

  }

  sealed trait AngularVelocityComponentOptics {

    def μasy[A]: Iso[Long, AngularVelocityComponent[A]] =
      Iso[Long, AngularVelocityComponent[A]](v =>
        AngularVelocityComponent(v.withUnit[MicroArcSecondPerYear])
      )(_.μasy.value)

    def microarcsecondsPerYear[A]: Iso[Long, AngularVelocityComponent[A]] = μasy

    /**
      * This `AngularVelocityComponent` as signed decimal milliseconds.
      */
    def milliarcsecondsPerYear[A]: SplitMono[AngularVelocityComponent[A], BigDecimal] =
      SplitMono
        .fromIso[AngularVelocityComponent[A], Long](microarcsecondsPerYear.reverse)
        .imapB(_.underlying.movePointRight(3).longValue,
               n => new java.math.BigDecimal(n).movePointLeft(3)
        )
  }

  type RA  = AngularVelocityComponent[VelocityAxis.RA]

  type Dec = AngularVelocityComponent[VelocityAxis.Dec]

  trait AngularVelocityComponentCompanion[A] {

    def apply(μasy: Quantity[Long, MicroArcSecondPerYear]): AngularVelocityComponent[A] =
      AngularVelocityComponent[A](μasy)

    val Zero: AngularVelocityComponent[A] =
      AngularVelocityComponent.Zero[A]

    val microarcsecondsPerYear: Iso[AngularVelocityComponent[A], Long] =
      AngularVelocityComponent.microarcsecondsPerYear[A].reverse

    val milliarcsecondsPerYear: SplitMono[AngularVelocityComponent[A], BigDecimal] =
      AngularVelocityComponent.milliarcsecondsPerYear[A]

  }

  object RA extends AngularVelocityComponentCompanion[VelocityAxis.RA]

  object Dec extends AngularVelocityComponentCompanion[VelocityAxis.Dec]

  /**
    * The `No parallax`
    * @group Constructors
    */
  val Zero: ProperVelocity =
    ProperVelocity(AngularVelocityComponent.Zero[VelocityAxis.RA],
                   AngularVelocityComponent.Zero[VelocityAxis.Dec]
    )

  /** @group Typeclass Instances */
  implicit val orderProperVelocity: Order[ProperVelocity] =
    Order.by(x => (x.ra, x.dec))

  /** @group Typeclass Instances */
  implicit val monoidProperVelocity: Monoid[ProperVelocity] =
    Monoid.instance(Zero, (a, b) => ProperVelocity(a.ra |+| b.ra, a.dec |+| b.dec))

}

sealed trait ProperVelocityOptics {

  /** @group Optics */
  val ra: Lens[ProperVelocity, AngularVelocityComponent[VelocityAxis.RA]] =
    GenLens[ProperVelocity](_.ra)

  /** @group Optics */
  val dec: Lens[ProperVelocity, AngularVelocityComponent[VelocityAxis.Dec]] =
    GenLens[ProperVelocity](_.dec)

  private def splitMonoFromComponents[A](
    raMono:  SplitMono[AngularVelocityComponent[VelocityAxis.RA], A],
    decMono: SplitMono[AngularVelocityComponent[VelocityAxis.Dec], A]
  ): SplitMono[ProperVelocity, (A, A)] =
    SplitMono(
      v => (raMono.get(v.ra), decMono.get(v.dec)),
      t => ProperVelocity(raMono.reverseGet(t._1), decMono.reverseGet(t._2))
    )

  /** @group Optics */
  val milliarcsecondsPerYear: SplitMono[ProperVelocity, (BigDecimal, BigDecimal)] =
    splitMonoFromComponents(
      ProperVelocity.RA.milliarcsecondsPerYear,
      ProperVelocity.Dec.milliarcsecondsPerYear
    )

}
