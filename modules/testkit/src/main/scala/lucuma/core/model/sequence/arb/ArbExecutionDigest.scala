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

  import ArbCategorizedTime.given
  import ArbSetupTime.given
  import ArbSequenceDigest.given

  given Arbitrary[ExecutionDigest] =
    Arbitrary {
      for {
        t <- arbitrary[SetupTime]
        c <- arbitrary[NonNegInt]
        r <- arbitrary[NonNegInt]
        i <- arbitrary[NonNegInt]
        x <- arbitrary[CategorizedTime]
        p <- arbitrary[NonNegInt]
        e <- arbitrary[CategorizedTime]
        a <- arbitrary[SequenceDigest]
        s <- arbitrary[SequenceDigest]
      } yield ExecutionDigest(t, c, r, i, x, p, e, a, s)
    }

  given Cogen[ExecutionDigest] =
    Cogen[(
      SetupTime,
      Int,
      Int,
      Int,
      CategorizedTime,
      Int,
      CategorizedTime,
      SequenceDigest,
      SequenceDigest
    )].contramap { a => (
      a.setup,
      a.setupCount.value,
      a.reacquisitionCount.value,
      a.existingCalibrationCount.value,
      a.existingCalibrationTime,
      a.expectedCalibrationCount.value,
      a.expectedCalibrationTime,
      a.acquisition,
      a.science
    )}
}

object ArbExecutionDigest extends ArbExecutionDigest
