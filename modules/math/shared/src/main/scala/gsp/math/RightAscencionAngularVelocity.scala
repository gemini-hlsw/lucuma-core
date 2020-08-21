// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats._
import coulomb._
import coulomb.cats.implicits._
import gsp.math.units._
import gsp.math.optics.SplitMono
import monocle.Iso
import spire.math.Rational
import spire.std.long._

/**
  * Representation of Right Ascension angular velocity in microarcseconds per year
  * It is used to represent proper velocity in RA
  */
final case class RightAscensionAngularVelocity(μasy: Quantity[Long, MicroArcSecondPerYear]) {
  val masy: Quantity[Rational, MilliArcSecondPerYear] = μasy.to[Rational, MilliArcSecondPerYear]

  override def toString =
    s"RightAscensionAngularVelocity(${masy.show})"
}

object RightAscensionAngularVelocity extends RightAscensionAngularVelocityOptics {

  /**
    * Zero RightAscensionAngularVelocity
    * @group Constructors
    */
  val Zero: RightAscensionAngularVelocity = new RightAscensionAngularVelocity(
    0.withUnit[MicroArcSecondPerYear]
  )

  /** @group Typeclass Instances */
  implicit val order: Order[RightAscensionAngularVelocity] =
    Order.by(_.μasy)

  /** @group Typeclass Instances */
  implicit val showRadialVelocity: Show[RightAscensionAngularVelocity] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val monoidProperVelocity: Monoid[RightAscensionAngularVelocity] =
    Monoid.instance(Zero, (a, b) => RightAscensionAngularVelocity(a.μasy + b.μasy))
}

sealed trait RightAscensionAngularVelocityOptics {

  val μasy: Iso[Long, RightAscensionAngularVelocity] =
    Iso[Long, RightAscensionAngularVelocity](v =>
      RightAscensionAngularVelocity(v.withUnit[MicroArcSecondPerYear])
    )(_.μasy.value)

  val microarcsecondsPerYear: Iso[Long, RightAscensionAngularVelocity] = μasy

  /**
    * This `RightAscensionAngularVelocity` as signed decimal milliseconds.
    */
  val milliarcsecondsPerYear: SplitMono[RightAscensionAngularVelocity, BigDecimal] =
    SplitMono
      .fromIso(microarcsecondsPerYear.reverse)
      .imapB(_.underlying.movePointRight(3).longValue,
             n => new java.math.BigDecimal(n).movePointLeft(3)
      )
}
