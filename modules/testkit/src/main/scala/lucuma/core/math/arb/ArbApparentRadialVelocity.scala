// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import coulomb.*
import coulomb.syntax.*
import lucuma.core.math.Constants.*
import lucuma.core.math.units.*
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbApparentRadialVelocity {

  // Bounded to a few times the speed of light: an arbitrary BigDecimal yields velocities such as
  // 1e300 m/s, which print as Infinity and cannot round trip through any formatter.
  given Arbitrary[ApparentRadialVelocity] =
    Arbitrary {
      for {
        cz <- Gen.chooseNum(-10L * SpeedOfLight.value, 10L * SpeedOfLight.value)
      } yield ApparentRadialVelocity(BigDecimal(cz).withUnit[MetersPerSecond])
    }

  given Cogen[ApparentRadialVelocity] =
    Cogen[BigDecimal].contramap(_.cz.value)
}

object ArbApparentRadialVelocity extends ArbApparentRadialVelocity
