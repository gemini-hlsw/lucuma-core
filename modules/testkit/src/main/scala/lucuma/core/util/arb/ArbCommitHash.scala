// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import lucuma.core.arb.*
import lucuma.core.util.CommitHash
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbCommitHash:

  given Arbitrary[CommitHash] =
    Arbitrary {
      Gen.listOfN(40, Gen.hexChar).map(lst => CommitHash.unsafeParse(lst.mkString))
    }

  given Cogen[CommitHash] =
    Cogen[String].contramap[CommitHash](_.format)

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],
      s => Gen.const(s.toUpperCase),
      s => Gen.const(s.replace("a", "A")),
      s => Gen.const(s"a$s")
    )

  val stringsCommitHash: Gen[String] =
    arbitrary[CommitHash]
      .map(_.format)
      .flatMapOneOf(Gen.const, perturbations*)

object ArbCommitHash extends ArbCommitHash
