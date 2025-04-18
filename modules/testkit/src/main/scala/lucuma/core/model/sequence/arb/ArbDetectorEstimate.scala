// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.types.numeric.NonNegInt
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen


trait ArbDetectorEstimate {
  import ArbDatasetEstimate.given

  given Arbitrary[DetectorEstimate] =
    Arbitrary {
      for {
        n <- arbitrary[String]
        d <- arbitrary[String]
        e <- arbitrary[DatasetEstimate]
        c <- Gen.chooseNum(0, Int.MaxValue)
      } yield DetectorEstimate(n, d, e, NonNegInt.unsafeFrom(c))
    }

  given Cogen[DetectorEstimate] =
    Cogen[(String, String, DatasetEstimate, Int)].contramap { a =>
      (a.name, a.description, a.dataset, a.count.value)
    }

}

object ArbDetectorEstimate extends ArbDetectorEstimate
