// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import coulomb._
import lucuma.core.math.units._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbApparentRadialVelocity {

  implicit val arbApparentRadialVelocity: Arbitrary[ApparentRadialVelocity] =
    Arbitrary {
      for {
        cz <- arbitrary[BigDecimal]
      } yield ApparentRadialVelocity(cz.withUnit[MetersPerSecond])
    }

  implicit val cogApparentRadialVelocity: Cogen[ApparentRadialVelocity] =
    Cogen[BigDecimal].contramap(_.cz.value)
}

object ArbApparentRadialVelocity extends ArbApparentRadialVelocity
