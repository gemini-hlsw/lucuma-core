// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import java.math.MathContext

import cats._
import coulomb._
import coulomb.cats.implicits._
import coulomb.si._
import lucuma.core.math.PhysicalConstants._
import lucuma.core.math.units._
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
    if (rv.value.abs < RadialVelocity.CValue) {
      val i = (rv / RadialVelocity.C).value
      val t = (1 + i) / (1 - i)
      Some(Redshift(BigDecimal.decimal(scala.math.sqrt(t.toDouble) - 1).round(rv.value.mc)))
    } else None

  override def toString =
    s"RadialVelocity(${rv.to[Double, KilometersPerSecond].show})"
}

object RadialVelocity {

  // Reference: https://www.nist.gov/si-redefinition/meet-constants
  // Exact value of the speed of light in m/s
  private val CValue: BigDecimal = BigDecimal.decimal(SpeedOfLight.toDouble, MathContext.DECIMAL64)

  val C: Quantity[BigDecimal, MetersPerSecond] =
    CValue.withUnit[MetersPerSecond].toUnit[MetersPerSecond] // Speed of light in m/s

  val fromMetersPerSecond: Prism[BigDecimal, RadialVelocity] =
    Prism[BigDecimal, RadialVelocity](b =>
      Some(b).filter(_.abs <= CValue).flatMap(v => RadialVelocity(v.withUnit[MetersPerSecond]))
    )(_.rv.value)

  /**
    * Construct a RadialVelocity if the value is in the allowed range
    * @group Constructors
    */
  def apply(rv: Quantity[BigDecimal, MetersPerSecond]): Option[RadialVelocity] =
    if (rv.value.abs < CValue) Some(new RadialVelocity(rv)) else None

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
