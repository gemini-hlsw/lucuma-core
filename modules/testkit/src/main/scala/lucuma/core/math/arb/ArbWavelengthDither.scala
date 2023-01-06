// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb


import cats.Eq
import coulomb.Quantity
import lucuma.core.math.units.Angstrom
import lucuma.core.math.units.Micrometer
import lucuma.core.math.units.Nanometer
import lucuma.core.math.units.Picometer
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

object ArbWavelengthDither {

  given Arbitrary[WavelengthDither] =
    Arbitrary {
      arbitrary[Int].map(WavelengthDither.intPicometers.get)
    }

  given Cogen[WavelengthDither] =
    Cogen[Int].contramap(_.toPicometers.value)

}
