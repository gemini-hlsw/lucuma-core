// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.scalacheck.numeric.given
import eu.timepit.refined.types.numeric.NonNegInt
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbExecutionDigest {

  import ArbCalibrationDigest.given
  import ArbSetupTime.given
  import ArbSequenceDigest.given

  given Arbitrary[ExecutionDigest] =
    Arbitrary {
      for {
        t <- arbitrary[SetupTime]
        c <- arbitrary[NonNegInt]
        r <- arbitrary[NonNegInt]
        n <- arbitrary[CalibrationDigest]
        a <- arbitrary[SequenceDigest]
        s <- arbitrary[SequenceDigest]
      } yield ExecutionDigest(t, c, r, n, a, s)
    }

  given Cogen[ExecutionDigest] =
    Cogen[(
      SetupTime,
      Int,
      Int,
      CalibrationDigest,
      SequenceDigest,
      SequenceDigest
    )].contramap { a => (
      a.setup,
      a.setupCount.value,
      a.reacquisitionCount.value,
      a.calibrations,
      a.acquisition,
      a.science
    )}
}

object ArbExecutionDigest extends ArbExecutionDigest
