// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import coulomb._
import coulomb.cats.implicits._
import lucuma.core.math.Constants.SpeedOfLight
import lucuma.core.math.units._
import lucuma.core.optics.Wedge
import monocle.Iso
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
  def toRedshift: Redshift = Redshift((cz / SpeedOfLight).value)

  override def toString =
    s"ApparentRadialVelocity(${cz.to[Double, KilometersPerSecond].show})"
}

object ApparentRadialVelocity {

  /**
    * Zero ApparentRadialVelocity
    * @group Constructors
    */
  val Zero: ApparentRadialVelocity = new ApparentRadialVelocity(0.withUnit[MetersPerSecond])

  /**
    * Iso to convert BigDecimal to ApparentRadialVelocity and viceversa
    * The value is assumed to be in m/s
    */
  val meterspersecond: Iso[BigDecimal, ApparentRadialVelocity] =
    Iso[BigDecimal, ApparentRadialVelocity](b =>
      ApparentRadialVelocity(b.withUnit[MetersPerSecond])
    )(cz => cz.cz.value)

  /**
    * Wedge to convert BigDecimal to ApparentRadialVelocity in kilometers per second
    */
  val kilometerspersecond: Wedge[BigDecimal, ApparentRadialVelocity] =
    Wedge[BigDecimal, ApparentRadialVelocity](
      b => ApparentRadialVelocity(b.withUnit[KilometersPerSecond]),
      cz => cz.cz.toUnit[KilometersPerSecond].value
    )

  /** @group Typeclass Instances */
  implicit val order: Order[ApparentRadialVelocity] =
    Order.by(_.cz)

  /** @group Typeclass Instances */
  implicit val showRadialVelocity: Show[ApparentRadialVelocity] =
    Show.fromToString

}
