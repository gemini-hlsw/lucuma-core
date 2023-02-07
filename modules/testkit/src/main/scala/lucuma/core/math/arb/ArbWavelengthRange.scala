// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.refined.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*
import org.scalacheck.*

trait ArbWavelengthRange:
  given Arbitrary[WavelengthRange] = Arbitrary(
    arbitrary[PosInt].map(WavelengthRange(_))
  )

  given Cogen[WavelengthRange] =
    Cogen[Int].contramap(_.toPicometers.value.value)

object ArbWavelengthRange extends ArbWavelengthRange
