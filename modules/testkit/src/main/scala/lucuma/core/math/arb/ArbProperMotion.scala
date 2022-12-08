// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import lucuma.core.math.ProperMotion
import lucuma.core.math.dimensional.*
import lucuma.core.util.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbProperMotion {
  import ProperMotion.*

  given arbAngularVelocityComponent[A]: Arbitrary[AngularVelocity Of A] =
    Arbitrary {
      arbitrary[Long].map(AngularVelocity.μasy[A].get)
    }

  given cogAngularVelocity[A]: Cogen[AngularVelocity Of A] =
    Cogen[Long].contramap(_.μasy.value)

  given Arbitrary[ProperMotion] =
    Arbitrary {
      for {
        ra  <- arbitrary[AngularVelocity Of VelocityAxis.RA]
        dec <- arbitrary[AngularVelocity Of VelocityAxis.Dec]
      } yield ProperMotion(ra, dec)
    }

  given Cogen[ProperMotion] =
    Cogen[
      (
        AngularVelocity Of VelocityAxis.RA,
        AngularVelocity Of VelocityAxis.Dec
      )
    ].contramap(x => (x.ra, x.dec))
}

object ArbProperMotion extends ArbProperMotion
