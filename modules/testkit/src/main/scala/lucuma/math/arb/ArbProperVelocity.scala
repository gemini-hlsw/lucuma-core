// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.math
package arb

import lucuma.math.ProperVelocity
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbProperVelocity {
  import ProperVelocity._

  implicit def arbAngularVelocityComponent[A]: Arbitrary[AngularVelocityComponent[A]] =
    Arbitrary {
      arbitrary[Long].map(AngularVelocityComponent.μasy[A].get)
    }

  implicit def cogAngularVelocity[A]: Cogen[AngularVelocityComponent[A]] =
    Cogen[Long].contramap(_.μasy.value)

  implicit val arbProperVelocity: Arbitrary[ProperVelocity] =
    Arbitrary {
      for {
        ra  <- arbitrary[AngularVelocityComponent[VelocityAxis.RA]]
        dec <- arbitrary[AngularVelocityComponent[VelocityAxis.Dec]]
      } yield ProperVelocity(ra, dec)
    }

  implicit val cogProperVelocity: Cogen[ProperVelocity] =
    Cogen[
      (
        AngularVelocityComponent[VelocityAxis.RA],
        AngularVelocityComponent[VelocityAxis.Dec]
      )
    ].contramap(x => (x.ra, x.dec))
}

object ArbProperVelocity extends ArbProperVelocity
