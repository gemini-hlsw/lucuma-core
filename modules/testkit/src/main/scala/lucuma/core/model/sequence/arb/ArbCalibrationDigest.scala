// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbCalibrationDigest:
  import ArbCalibrationEstimate.given

  given Arbitrary[CalibrationDigest] =
    Arbitrary:
      for
        x <- arbitrary[CalibrationEstimate]
        e <- arbitrary[CalibrationEstimate]
      yield CalibrationDigest(x, e)

  given Cogen[CalibrationDigest] =
    Cogen[(CalibrationEstimate, CalibrationEstimate)].contramap(a => (a.existing, a.expected))

object ArbCalibrationDigest extends ArbCalibrationDigest
