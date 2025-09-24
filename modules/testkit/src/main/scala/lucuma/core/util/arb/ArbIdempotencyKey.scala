// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package arb

import lucuma.core.arb.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

import java.util.UUID

trait ArbIdempotencyKey:
  given arbIdempotencyKey: Arbitrary[IdempotencyKey] =
    Arbitrary(arbitrary[UUID].map(IdempotencyKey.apply))

  given cogIdempotencyKey: Cogen[IdempotencyKey] =
    Cogen[UUID].contramap(_.value)

  def uuidStringPerturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],
      s => Gen.const(s.toLowerCase),
      s => Gen.const(s.toUpperCase)
    )

  def uuidStrings: Gen[String] =
    arbitrary[UUID]
      .map(_.toString)
      .flatMapOneOf(Gen.const, uuidStringPerturbations*)

object ArbIdempotencyKey extends ArbIdempotencyKey