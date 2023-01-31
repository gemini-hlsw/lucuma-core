// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.refined._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbWavelengthCoverage:
  given Arbitrary[WavelengthCoverage] = Arbitrary(
    arbitrary[PosInt].map(WavelengthCoverage(_))
  )

  given Cogen[WavelengthCoverage] =
    Cogen[Int].contramap(_.toPicometers.value.value)

object ArbWavelengthCoverage extends ArbWavelengthCoverage
