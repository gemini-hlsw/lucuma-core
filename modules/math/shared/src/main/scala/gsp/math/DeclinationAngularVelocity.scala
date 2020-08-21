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
  * Representation of Declination angular velocity in microarcseconds per year
  * It is used to represent proper velocity in Dec
  */
final case class DeclinationAngularVelocity(μasy: Quantity[Long, MicroArcSecondPerYear]) {
  val masy: Quantity[Rational, MilliArcSecondPerYear] = μasy.to[Rational, MilliArcSecondPerYear]

  override def toString =
    s"DeclinationAngularVelocity(${masy.show})"
}

object DeclinationAngularVelocity extends DeclinationAngularVelocityOptics {

  /**
    * Zero DeclinationAngularVelocity
    * @group Constructors
    */
  val Zero: DeclinationAngularVelocity = new DeclinationAngularVelocity(
    0.withUnit[MicroArcSecondPerYear]
  )

  /** @group Typeclass Instances */
  implicit val order: Order[DeclinationAngularVelocity] =
    Order.by(_.μasy)

  /** @group Typeclass Instances */
  implicit val showRadialVelocity: Show[DeclinationAngularVelocity] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val monoidProperVelocity: Monoid[DeclinationAngularVelocity] =
    Monoid.instance(Zero, (a, b) => DeclinationAngularVelocity(a.μasy + b.μasy))

}

sealed trait DeclinationAngularVelocityOptics {

  val μasy: Iso[Long, DeclinationAngularVelocity] =
    Iso[Long, DeclinationAngularVelocity](v =>
      DeclinationAngularVelocity(v.withUnit[MicroArcSecondPerYear])
    )(_.μasy.value)

  val microarcsecondsPerYear: Iso[Long, DeclinationAngularVelocity] = μasy

  /**
    * This `DeclinationAngularVelocity` as signed decimal milliseconds.
    */
  val milliarcsecondsPerYear: SplitMono[DeclinationAngularVelocity, BigDecimal] =
    SplitMono
      .fromIso(microarcsecondsPerYear.reverse)
      .imapB(_.underlying.movePointRight(3).longValue,
             n => new java.math.BigDecimal(n).movePointLeft(3)
      )
}
