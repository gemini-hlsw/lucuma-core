// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.types.numeric.PosInt
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen


trait ArbObservationReference extends ArbReference {

  import ArbProgramReference.given

  given Arbitrary[ObservationReference] =
    Arbitrary {
      for {
        p <- arbitrary[ProgramReference]
        n <- arbitraryIndex
      } yield ObservationReference(p, PosInt.unsafeFrom(n))
    }

  given Cogen[ObservationReference] =
    Cogen[(ProgramReference, Int)].contramap { a => (
      a.programReference,
      a.observationIndex.value
    )}

  val observationReferenceStrings: Gen[String] =
    referenceStrings[ObservationReference](_.label)

}

object ArbObservationReference extends ArbObservationReference