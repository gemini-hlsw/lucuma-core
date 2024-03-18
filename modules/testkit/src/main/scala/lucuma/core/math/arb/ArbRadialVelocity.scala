// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import lucuma.core.math.Constants._
import lucuma.core.math.units._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbRadialVelocity {

  given Arbitrary[RadialVelocity] =
    Arbitrary {
      for {
        rv <- Gen.chooseNum(-SpeedOfLight.value + 1, SpeedOfLight.value - 1)
      } yield RadialVelocity.unsafeFromQuantity(rv.withUnit[MetersPerSecond].toValue[BigDecimal])
    }

  given Cogen[RadialVelocity] =
    Cogen[BigDecimal].contramap(_.rv.value)
}

object ArbRadialVelocity extends ArbRadialVelocity
