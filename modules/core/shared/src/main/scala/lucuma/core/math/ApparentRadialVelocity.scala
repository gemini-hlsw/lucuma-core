// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import algebra.instances.all.given
import cats.*
import coulomb.*
import coulomb.conversion.*
import coulomb.conversion.implicits.given
import coulomb.integrations.cats.quantity.given
import coulomb.syntax.*
import coulomb.units.constants.SpeedOfLight
import coulomb.units.constants.constant
import lucuma.core.math.units.*
import lucuma.core.optics.Wedge
import monocle.Iso

import scala.language.implicitConversions

/**
  * Representation of a radial velocity in meters per second
  * Unlike RadialVelocity this is not limited to the speed of light
  * Apparent Radial Velocity is often represented as cz
  */
final case class ApparentRadialVelocity(cz: Quantity[BigDecimal, MetersPerSecond]) {

  /**
    * Converts the apparent radial velocity to a Redshift
    */
  def toRedshift: Redshift = Redshift((cz / constant[BigDecimal, SpeedOfLight]).value)

  override def toString =
    s"ApparentRadialVelocity(${cz.toUnit[KilometersPerSecond].toValue[Double].show})"
}

object ApparentRadialVelocity {

  /**
    * Zero ApparentRadialVelocity
    * @group Constructors
    */
  val Zero: ApparentRadialVelocity = new ApparentRadialVelocity(0.withUnit[MetersPerSecond].toValue[BigDecimal])

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
  given Order[ApparentRadialVelocity] =
    Order.by(_.cz)

  /** @group Typeclass Instances */
  given Show[ApparentRadialVelocity] =
    Show.fromToString

}
