// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math
package arb

import coulomb._
import gsp.math.units._
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

  implicit val cogRedshift: Cogen[ApparentRadialVelocity] =
    Cogen[BigDecimal].contramap(_.cz.value)
}

object ArbApparentRadialVelocity extends ArbApparentRadialVelocity
