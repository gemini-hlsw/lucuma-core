// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package arb

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbCalculatedValue:
  import ArbEnumerated.given

  given [A: Arbitrary]: Arbitrary[CalculatedValue[A]] =
    Arbitrary:
      for
        s <- arbitrary[CalculationState]
        a <- arbitrary[A]
      yield CalculatedValue(s, a)

  given [A: Cogen]: Cogen[CalculatedValue[A]] =
    Cogen[(CalculationState, A)].contramap: cv =>
      (cv.state, cv.value)

object ArbCalculatedValue extends ArbCalculatedValue