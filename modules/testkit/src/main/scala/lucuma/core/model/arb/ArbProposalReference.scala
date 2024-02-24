// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.types.numeric.PosInt
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen


trait ArbProposalReference extends ArbReference {

  import ArbSemester.given

  given Arbitrary[ProposalReference] =
    Arbitrary {
      for {
        s <- arbitrary[Semester]
        i <- arbitraryIndex
      } yield ProposalReference(s, PosInt.unsafeFrom(i))
    }

  given Cogen[ProposalReference] =
    Cogen[(Semester, Int)].contramap { a => (
      a.semester,
      a.index.value
    )}

  val proposalReferenceStrings: Gen[String] =
    referenceStrings[ProposalReference](_.label)

}

object ArbProposalReference extends ArbProposalReference