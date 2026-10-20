// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.ObservationValidationCode
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbStepViolation:
  import ArbEnumerated.given

  given Arbitrary[StepViolation] =
    Arbitrary:
      for
        c <- arbitrary[ObservationValidationCode]
        m <- Gen.alphaStr
      yield StepViolation(c, m)

  given Cogen[StepViolation] =
    Cogen[(ObservationValidationCode, String)].contramap: a =>
      (a.code, a.message)

object ArbStepViolation extends ArbStepViolation
