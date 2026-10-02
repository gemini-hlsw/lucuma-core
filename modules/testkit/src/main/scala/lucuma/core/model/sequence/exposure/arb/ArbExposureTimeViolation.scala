// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.exposure
package arb

import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbExposureTimeViolation:
  import ArbEnumerated.given

  given Arbitrary[ExposureTimeViolation] =
    Arbitrary:
      for
        s <- arbitrary[ExposureTimeViolation.Severity]
        d <- Gen.alphaStr
      yield ExposureTimeViolation(s, d)

  given Cogen[ExposureTimeViolation] =
    Cogen[(ExposureTimeViolation.Severity, String)].contramap: a =>
      (a.severity, a.description)

object ArbExposureTimeViolation extends ArbExposureTimeViolation
