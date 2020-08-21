// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats._
import cats.implicits._
import coulomb._
import coulomb.accepted._
import gsp.math.units._
import monocle.Lens
import monocle.macros.GenLens

/**
  * ProperVelocity contains Ra/Dec angular velocities
  */
final case class ProperVelocity(
  ra:  RightAscensionAngularVelocity,
  dec: DeclinationAngularVelocity
) {
  // Return the ra/dec components in radians, first converting to degrees/y
  def toRadians: (Double, Double) =
    (ra.μasy.to[Double, Degree %/ Year].value, dec.μasy.to[Double, Degree %/ Year].value)
      .map(_.toRadians)

}

object ProperVelocity extends ProperVelocityOptics {

  /**
    * The `No parallax`
    * @group Constructors
    */
  val Zero: ProperVelocity =
    ProperVelocity(RightAscensionAngularVelocity.Zero, DeclinationAngularVelocity.Zero)

  /** @group Typeclass Instances */
  implicit val orderProperVelocity: Order[ProperVelocity] =
    Order.by(x => (x.ra, x.dec))

  /** @group Typeclass Instances */
  implicit val monoidProperVelocity: Monoid[ProperVelocity] =
    Monoid.instance(Zero, (a, b) => ProperVelocity(a.ra |+| b.ra, a.dec |+| b.dec))

}

sealed trait ProperVelocityOptics {

  /** @group Optics */
  val ra: Lens[ProperVelocity, RightAscensionAngularVelocity] =
    GenLens[ProperVelocity](_.ra)

  /** @group Optics */
  val dec: Lens[ProperVelocity, DeclinationAngularVelocity] =
    GenLens[ProperVelocity](_.dec)

}
