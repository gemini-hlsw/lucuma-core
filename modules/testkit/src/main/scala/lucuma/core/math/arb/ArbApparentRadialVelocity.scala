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
  // 1e300 m/s, which print as Infinity and cannot round trip through any formatter. Half of the
  // values carry 40 significant digits, more than the DECIMAL128 context of a unit conversion, so
  // converting them rounds, which the Wedge coverage laws require.
  given Arbitrary[ApparentRadialVelocity] =
    Arbitrary {
      val bound: Long = 10L * SpeedOfLight.value
      val integral: Gen[BigDecimal] =
        Gen.chooseNum(-bound, bound).map(BigDecimal(_))
      val precise: Gen[BigDecimal]  =
        for {
          cz   <- Gen.chooseNum(-bound, bound)
          frac <- Gen.listOfN(30, Gen.numChar).map(_.mkString)
        } yield BigDecimal(new java.math.BigDecimal(s"$cz.$frac"))
      Gen.oneOf(integral, precise).map(cz => ApparentRadialVelocity(cz.withUnit[MetersPerSecond]))
    }

  given Cogen[ApparentRadialVelocity] =
    Cogen[BigDecimal].contramap(_.cz.value)
}

object ArbApparentRadialVelocity extends ArbApparentRadialVelocity
