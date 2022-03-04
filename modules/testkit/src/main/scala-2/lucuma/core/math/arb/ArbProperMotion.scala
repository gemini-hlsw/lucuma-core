// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import lucuma.core.math.ProperMotion
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbProperMotion {
  import ProperMotion._

  implicit def arbAngularVelocityComponent[A]: Arbitrary[AngularVelocityComponent[A]] =
    Arbitrary {
      arbitrary[Long].map(AngularVelocityComponent.μasy[A].get)
    }

  implicit def cogAngularVelocity[A]: Cogen[AngularVelocityComponent[A]] =
    Cogen[Long].contramap(_.μasy.value)

  implicit val arbProperMotion: Arbitrary[ProperMotion] =
    Arbitrary {
      for {
        ra  <- arbitrary[AngularVelocityComponent[VelocityAxis.RA]]
        dec <- arbitrary[AngularVelocityComponent[VelocityAxis.Dec]]
      } yield ProperMotion(ra, dec)
    }

  implicit val cogProperMotion: Cogen[ProperMotion] =
    Cogen[
      (
        AngularVelocityComponent[VelocityAxis.RA],
        AngularVelocityComponent[VelocityAxis.Dec]
      )
    ].contramap(x => (x.ra, x.dec))
}

object ArbProperMotion extends ArbProperMotion
