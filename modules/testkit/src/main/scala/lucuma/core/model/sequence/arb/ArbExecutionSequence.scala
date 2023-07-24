// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.arb.ArbBoundedCollection
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbExecutionSequence {
  import ArbAtom.given
  import ArbBoundedCollection.*

  given [D: Arbitrary]: Arbitrary[ExecutionSequence[D]] =
    Arbitrary {
      for {
        as <- genBoundedNonEmptyList[Atom[D]](BoundedCollectionLimit)
        m  <- arbitrary[Boolean]
        n  <- Gen.posNum[Int].map(_ max as.length).map(PosInt.unsafeFrom)
      } yield ExecutionSequence(as.head, as.tail, m, n)
    }

  given [D: Cogen]: Cogen[ExecutionSequence[D]] =
    Cogen[(
      Atom[D],
      List[Atom[D]],
      Boolean,
      Int
    )].contramap { a => (
      a.nextAtom,
      a.possibleFuture,
      a.hasMore,
      a.atomCount.value
    )}

}

object ArbExecutionSequence extends ArbExecutionSequence
