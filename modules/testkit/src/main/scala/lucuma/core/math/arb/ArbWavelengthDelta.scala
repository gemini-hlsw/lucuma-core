// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosInt
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbWavelengthDelta:
  given Arbitrary[WavelengthDelta] = Arbitrary(
    arbitrary[PosInt].map(WavelengthDelta(_))
  )

  given Cogen[WavelengthDelta] =
    Cogen[Int].contramap(_.toPicometers.value.value)

object ArbWavelengthDelta extends ArbWavelengthDelta
