// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import cats.data.NonEmptyList
import cats.syntax.all._
import lucuma.core.data.Zipper
import lucuma.core.util.arb.ArbBoundedCollection
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbSequence {
  import ArbAtom.given
  import ArbBoundedCollection.*
  import ArbUid._

  given [D: Arbitrary]: Arbitrary[Sequence[D]] =
    Arbitrary {
      genBoundedNonEmptyList[Atom[D]](BoundedCollectionLimit).map(Sequence(_))
    }

  given [D: Cogen]: Cogen[Sequence[D]] =
    Cogen[List[Atom[D]]].contramap(_.atoms.toList)

}

object ArbSequence extends ArbSequence
