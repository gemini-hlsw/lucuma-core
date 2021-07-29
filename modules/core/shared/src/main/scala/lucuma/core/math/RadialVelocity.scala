// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import coulomb._
import coulomb.cats.implicits._
import coulomb.si._
import lucuma.core.math.Constants.SpeedOfLight
import lucuma.core.math.units._
import lucuma.core.optics.Format
import monocle.Prism
import spire.std.bigDecimal._

/**
  * Representation of a radial velocity in meters per second
  * Valid range is (-C, C) where C is the speed of light
  * Radial Velocity is often represented as RV
  *
  * The selection of units is based on references to velocities as low as 20 cm/s
  * https://en.wikipedia.org/wiki/Radial_velocity
  */
final case class RadialVelocity private (rv: Quantity[BigDecimal, MetersPerSecond]) {

  def toDoubleKilometersPerSecond: Double = rv.to[Double, KilometersPerSecond].value

  /**
    * Converts the radial velocity to a Redshift, approximate
    * a return value of None should be understood as an infinity Redshift
    */
  def toRedshift: Option[Redshift] =
    if (rv.value.abs < SpeedOfLight.value) {
      val i = (rv / SpeedOfLight).value
      val t = (1 + i) / (1 - i)
      Some(Redshift(BigDecimal.decimal(scala.math.sqrt(t.toDouble) - 1).round(rv.value.mc)))
    } else None

  override def toString =
    s"RadialVelocity(${rv.to[Double, KilometersPerSecond].show})"
}

object RadialVelocity {

  val fromMetersPerSecond: Prism[BigDecimal, RadialVelocity] =
    Prism[BigDecimal, RadialVelocity](b =>
      Some(b)
        .filter(_.abs <= SpeedOfLight.value)
        .flatMap(v => RadialVelocity(v.withUnit[MetersPerSecond]))
    )(_.rv.value)

  val kilometerspersecond: Format[BigDecimal, RadialVelocity] =
    Format[BigDecimal, RadialVelocity](
      b =>
        Some(b)
          .filter(_.abs <= SpeedOfLight.to[BigDecimal, KilometersPerSecond].value)
          .flatMap(v => RadialVelocity(v.withUnit[KilometersPerSecond])),
      rv => rv.rv.toUnit[KilometersPerSecond].value
    )

  /**
    * Construct a RadialVelocity if the value is in the allowed range
    * @group Constructors
    */
  def apply(rv: Quantity[BigDecimal, MetersPerSecond]): Option[RadialVelocity] =
    if (rv.value.abs < SpeedOfLight.value) Some(new RadialVelocity(rv)) else None

  /**
    * Attempts to construct a RadialVelocity, it will fail if the value is outside the allowed range
    * @group Constructors
    */
  def unsafeFromQuantity(rv: Quantity[BigDecimal, MetersPerSecond]): RadialVelocity =
    apply(rv).getOrElse(sys.error(s"Value of rv $rv not allowed"))

  /**
    * `Zero RadialVelocity`
    * @group Constructors
    */
  val Zero: RadialVelocity = new RadialVelocity(0.withUnit[MetersPerSecond])

  /** @group Typeclass Instances */
  implicit val orderRadialVelocity: Order[RadialVelocity] =
    Order.by(_.rv)

  /** @group Typeclass Instances */
  implicit val showRadialVelocity: Show[RadialVelocity] =
    Show.fromToString

}
