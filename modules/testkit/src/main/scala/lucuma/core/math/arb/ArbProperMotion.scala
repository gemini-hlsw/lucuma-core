// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import lucuma.core.util.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbProperMotion {
  import ProperMotion.*

  given arbAngularVelocityComponent[A]: Arbitrary[AngularVelocity Of A] =
    Arbitrary {
      arbitrary[Long].map(μasyVelocity[A])
    }

  given cogAngularVelocity[A]: Cogen[AngularVelocity Of A] =
    Cogen[Long].contramap(_.μasy.value)

  given Arbitrary[ProperMotion] =
    Arbitrary {
      for {
        ra  <- arbitrary[ProperMotion.RA]
        dec <- arbitrary[ProperMotion.Dec]
      } yield ProperMotion(ra, dec)
    }

  given Cogen[ProperMotion] =
    Cogen[
      (
        ProperMotion.RA,
        ProperMotion.Dec
      )
    ].contramap(x => (x.ra, x.dec))
}

object ArbProperMotion extends ArbProperMotion
