// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbDatasetEstimate {
  import ArbTimeSpan.given

  given Arbitrary[DatasetEstimate] =
    Arbitrary {
      for {
        e <- arbitrary[TimeSpan]
        r <- arbitrary[TimeSpan]
        w <- arbitrary[TimeSpan]
      } yield DatasetEstimate(e, r, w)
    }

  given Cogen[DatasetEstimate] =
    Cogen[(TimeSpan, TimeSpan, TimeSpan)].contramap { a =>
      (a.exposure, a.readout, a.write)
    }

}

object ArbDatasetEstimate extends ArbDatasetEstimate
