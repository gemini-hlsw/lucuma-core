// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable.arb

import cats.data.NonEmptyList
import eu.timepit.refined.scalacheck.string.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.votable.*
import org.scalacheck.*

trait ArbUcd {
  val genNonEmptyString = summon[Arbitrary[NonEmptyString]].arbitrary

  given Arbitrary[Ucd] =
    Arbitrary {
      for {
        a <- Gen.nonEmptyListOf[NonEmptyString](genNonEmptyString)
      } yield Ucd(NonEmptyList.fromList(a).get)
    }

  given Cogen[Ucd] =
    Cogen[List[String]].contramap(_.tokens.map(_.value).toList)

}

object ArbUcd extends ArbUcd
