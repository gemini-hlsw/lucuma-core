// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.math

import cats._
import coulomb._
import coulomb.cats.implicits._
import lucuma.math.units._
import spire.std.bigDecimal._

/**
  * Representation of a radial velocity in meters per second
  * Unlike RadialVelocity this is not limited to the speed of light
  * Apparent Radial Velocity is often represented as cz
  */
final case class ApparentRadialVelocity(cz: Quantity[BigDecimal, MetersPerSecond]) {

  /**
    * Converts the apparent radial velocity to a Redshift
    */
  def toRedshift: Redshift = Redshift((cz / RadialVelocity.C).value)

  override def toString =
    s"ApparentRadialVelocity(${cz.to[Double, KilometersPerSecond].show})"
}

object ApparentRadialVelocity {

  /**
    * Zero ApparentRadialVelocity
    * @group Constructors
    */
  val Zero: ApparentRadialVelocity = new ApparentRadialVelocity(0.withUnit[MetersPerSecond])

  /** @group Typeclass Instances */
  implicit val order: Order[ApparentRadialVelocity] =
    Order.by(_.cz)

  /** @group Typeclass Instances */
  implicit val showRadialVelocity: Show[ApparentRadialVelocity] =
    Show.fromToString

}
