// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package arb

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbCalculationValue:
  import ArbEnumerated.given

  given [A: Arbitrary]: Arbitrary[CalculationValue[A]] =
    Arbitrary:
      for
        p <- arbitrary[CalculationPhase]
        a <- arbitrary[A]
      yield CalculationValue(p, a)

  given [A: Cogen]: Cogen[CalculationValue[A]] =
    Cogen[(CalculationPhase, A)].contramap: cv =>
      (cv.phase, cv.value)

object ArbCalculationValue extends ArbCalculationValue