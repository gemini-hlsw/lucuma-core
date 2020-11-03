// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import coulomb._
import lucuma.core.math.Constants.SpeedOfLight
import lucuma.core.math.units._

/**
  * Represents a redshift of an object as it moves away (positive) or towards (negative) the observing point
  *
  * Redshift can be (perhaps surprisingly) higher than 1.
  *
  * For far objects Redshift can be converted to RadialVelocity which takes into account relativistic effects and cannot be more than C
  * For nearer objects we can convert to ApparentRadialVelocity which doesn't consider relativistic effects
  *
  * Often Redshift is referred as z
  */
final case class Redshift(z: BigDecimal) {

  /**
    * Converts to RadialVelocity, approximate
    */
  def toRadialVelocity: Option[RadialVelocity] = {
    val rv = SpeedOfLight.value * (((z + 1) * (z + 1) - 1) / ((z + 1) * (z + 1) + 1))
    RadialVelocity(rv.round(z.mc).withUnit[MetersPerSecond])
  }

  def toApparentRadialVelocity: ApparentRadialVelocity =
    ApparentRadialVelocity((SpeedOfLight.value * z).withUnit[MetersPerSecond])

  override def toString =
    s"Redshift($z)"
}

object Redshift {

  /**
    * The `No redshift`
    * @group Constructors
    */
  val Zero: Redshift = Redshift(0)

  /** @group Typeclass Instances */
  implicit val orderRedshift: Order[Redshift] =
    Order.by(_.z)

  /** @group Typeclass Instances */
  implicit val showRedshift: Show[Redshift] =
    Show.fromToString

}
