// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.{ Order, Show }
import cats.implicits._
import gsp.math.PhysicalConstants.SpeedOfLight
import gsp.math.optics.Format
import gsp.math.syntax.prism._
import gsp.math.units._
import monocle.Prism
import coulomb._
import coulomb.cats.implicits._
import spire.math.Rational

/**
  * Radial Velocity represented as integral meters per second, positive if receding. We can also
  * view velocity in terms of redshift (a dimensionless value called '''z''').
  * @see Wikipedia on [[https://en.wikipedia.org/wiki/Radial_velocity Radial Velocity]]
  * @see Wikipedia on [[https://en.wikipedia.org/wiki/Redshift Redshift]]
  */
sealed abstract case class RadialVelocity(toMetersPerSecond: Quantity[Int, MetersPerSecond]) {

  // Sanity check
  assert(toMetersPerSecond.value.abs <= SpeedOfLight, "Radial velocity exceeds the speed of light.")

  def toKilometersPerSecond: Quantity[Rational, KilometersPerSecond] =
    toMetersPerSecond.to[Rational, KilometersPerSecond]

  def toDoubleKilometersPerSecond: Double =
    toKilometersPerSecond.toValue[Double].value

  override def toString =
    s"RadialVelocity(${toMetersPerSecond.to[Double, KilometersPerSecond].show})"

}

object RadialVelocity {

  val fromMetersPerSecond: Prism[Int, RadialVelocity] =
    Prism[Int, RadialVelocity](
      _.some
        .filter(_.abs <= SpeedOfLight)
        .map(v => new RadialVelocity(v.withUnit[MetersPerSecond]) {})
    )(
      _.toMetersPerSecond.value
    )

  val fromKilometersPerSecond: Format[BigDecimal, RadialVelocity] =
    Format
      .fromPrism(fromMetersPerSecond)
      .imapA(
        n => new java.math.BigDecimal(n).movePointLeft(3),
        d => d.underlying.movePointRight(3).intValue
      )

  /** Radial velocity of zero. */
  val Zero: RadialVelocity = fromMetersPerSecond.unsafeGet(0)

  /** Construct a [[RadialVelocity]] from floating point kilometers per second. */
  def unsafeFromDoubleKilometersPerSecond(kms: Double): RadialVelocity =
    fromKilometersPerSecond.unsafeGet(BigDecimal(kms))

  /** Instances are ordered by their `.toMetersPerSecond` values. */
  implicit val RadialVelocityOrder: Order[RadialVelocity] =
    Order.by(_.toMetersPerSecond)

  implicit val RadialVelocityShow: Show[RadialVelocity] =
    Show.fromToString

}
