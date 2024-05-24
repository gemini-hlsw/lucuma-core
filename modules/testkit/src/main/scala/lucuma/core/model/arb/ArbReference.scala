// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.arb.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

trait ArbReference {

  val arbitraryIndex: Gen[Int] =
    Gen.oneOf(
      Gen.choose(   1,    9),
      Gen.choose(  10,   99),
      Gen.choose( 100,  999),
      Gen.choose(1000, 9999),
      Gen.const(      10000)
    )

  private val referenceStringPerturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],
      s => Gen.const(s.toLowerCase),
      s => Gen.const(s.replaceFirst("-0", "-")),
      s => Gen.const(s.replaceFirst("-0", "-00"))
    )

  def referenceStrings[A: Arbitrary](f: A => String): Gen[String] =
    arbitrary[A].map(f).flatMapOneOf(Gen.const, referenceStringPerturbations*)

}
