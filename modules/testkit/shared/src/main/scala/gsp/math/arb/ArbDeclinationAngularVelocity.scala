// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math
package arb

import gsp.math.DeclinationAngularVelocity
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbDeclinationAngularVelocity {

  implicit val arbDeclinationAngularVelocity: Arbitrary[DeclinationAngularVelocity] =
    Arbitrary {
      arbitrary[Long].map(DeclinationAngularVelocity.μasy.get)
    }

  implicit val cogDeclinationAngularVelocity: Cogen[DeclinationAngularVelocity] =
    Cogen[Long].contramap(_.μasy.value)
}

object ArbDeclinationAngularVelocity extends ArbDeclinationAngularVelocity
