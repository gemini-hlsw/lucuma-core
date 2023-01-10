// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional.arb

import coulomb.Unitless
import lucuma.core.math.dimensional.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.*

trait ArbUnits {
  given Arbitrary[Units] =
    Arbitrary {
      for {
        name       <- arbitrary[String]
        abbv       <- arbitrary[String]
        serialized <- arbitrary[String]
      } yield UnitOfMeasure[1](name, abbv, serialized)
    }

  given Cogen[Units] =
    Cogen[(String, String, String)].contramap(x => (x.name, x.abbv, x.serialized))
}

object ArbUnits extends ArbUnits
