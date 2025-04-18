// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.ObservationReference
import lucuma.core.model.arb.ArbObservationReference
import lucuma.core.model.arb.ArbReference
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen


trait ArbDatasetReference extends ArbReference {

  import ArbObservationReference.given

  given Arbitrary[DatasetReference] =
    Arbitrary {
      for {
        p <- arbitrary[ObservationReference]
        s <- arbitraryIndex
        e <- arbitraryIndex
      } yield DatasetReference(p, PosInt.unsafeFrom(s), PosInt.unsafeFrom(e))
    }

  given Cogen[DatasetReference] =
    Cogen[(ObservationReference, Int, Int)].contramap { a => (
      a.observationReference,
      a.stepIndex.value,
      a.exposureIndex.value
    )}

  val datasetReferenceStrings: Gen[String] =
    referenceStrings[DatasetReference](_.label)

}

object ArbDatasetReference extends ArbDatasetReference

