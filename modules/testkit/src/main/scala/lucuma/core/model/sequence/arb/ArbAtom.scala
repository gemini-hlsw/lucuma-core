// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.data.Zipper
import lucuma.core.util.arb.ArbBoundedCollection
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbAtom {
  import ArbBoundedCollection.*
  import ArbStep.given
  import ArbUid._

  given [D: Arbitrary]: Arbitrary[Atom[D]] =
    Arbitrary {
      for {
        i <- arbitrary[Atom.Id]
        d <- Gen.alphaStr.map(s => NonEmptyString.from(s).toOption)
        s <- genBoundedNonEmptyList[Step[D]](BoundedCollectionLimit)
      } yield Atom(i, d, s)
    }

  given [D: Cogen]: Cogen[Atom[D]] =
    Cogen[(Atom.Id, Option[String], List[Step[D]])].contramap { a =>
      (a.id, a.description.map(_.value), a.steps.toList)
    }

}

object ArbAtom extends ArbAtom