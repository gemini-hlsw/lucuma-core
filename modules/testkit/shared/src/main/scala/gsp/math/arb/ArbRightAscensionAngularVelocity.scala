// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math
package arb

import gsp.math.RightAscensionAngularVelocity
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbRightAscensionAngularVelocity {

  implicit val arbRightAscensionAngularVelocity: Arbitrary[RightAscensionAngularVelocity] =
    Arbitrary {
      arbitrary[Long].map(RightAscensionAngularVelocity.μasy.get)
    }

  implicit val cogRightAscensionAngularVelocity: Cogen[RightAscensionAngularVelocity] =
    Cogen[Long].contramap(_.μasy.value)
}

object ArbRightAscensionAngularVelocity extends ArbRightAscensionAngularVelocity
