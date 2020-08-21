// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math
package arb

import gsp.math.ProperVelocity
import gsp.math.RightAscensionAngularVelocity
import gsp.math.DeclinationAngularVelocity
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbProperVelocity {
  import ArbRightAscensionAngularVelocity._
  import ArbDeclinationAngularVelocity._

  implicit val arbProperVelocity: Arbitrary[ProperVelocity] =
    Arbitrary {
      for {
        ra  <- arbitrary[RightAscensionAngularVelocity]
        dec <- arbitrary[DeclinationAngularVelocity]
      } yield ProperVelocity(ra, dec)
    }

  implicit val cogProperVelocity: Cogen[ProperVelocity] =
    Cogen[(RightAscensionAngularVelocity, DeclinationAngularVelocity)].contramap(x => (x.ra, x.dec))
}

object ArbProperVelocity extends ArbProperVelocity
