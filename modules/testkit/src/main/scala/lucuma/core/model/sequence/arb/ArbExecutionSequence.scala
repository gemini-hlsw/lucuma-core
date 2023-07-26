// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.util.arb.ArbBoundedCollection
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbExecutionSequence {
  import ArbAtom.given
  import ArbBoundedCollection.*

  given [D: Arbitrary]: Arbitrary[ExecutionSequence[D]] =
    Arbitrary {
      for {
        as <- genBoundedNonEmptyList[Atom[D]](BoundedCollectionLimit)
        m  <- arbitrary[Boolean]
      } yield ExecutionSequence(as.head, as.tail, m)
    }

  given [D: Cogen]: Cogen[ExecutionSequence[D]] =
    Cogen[(
      Atom[D],
      List[Atom[D]],
      Boolean
    )].contramap { a => (
      a.nextAtom,
      a.possibleFuture,
      a.hasMore
    )}

}

object ArbExecutionSequence extends ArbExecutionSequence
