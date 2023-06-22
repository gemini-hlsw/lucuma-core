// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb


import coulomb.Quantity
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

object ArbWavelengthDither {

  given Arbitrary[WavelengthDither] =
    Arbitrary {
      arbitrary[Int].map(WavelengthDither.intPicometers.get)
    }

  given Cogen[WavelengthDither] =
    Cogen[Int].contramap(_.toPicometers.value)

}
