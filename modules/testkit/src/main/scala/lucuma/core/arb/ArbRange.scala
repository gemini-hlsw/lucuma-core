// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.arb

import cats.collections.Range
import cats.kernel.Order
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbRange:

  def genRange[A: Arbitrary: Order]: Gen[Range[A]] =
    arbitrary[(A, A)].map:
      case (a, b) => 
        if (a <= b) then Range(a, b) else Range(b, a)

  given [A: Arbitrary: Order]: Arbitrary[Range[A]] =
    Arbitrary(genRange)

  given [A: Cogen]: Cogen[Range[A]] =
    Cogen[(A, A)].contramap: 
      r => (r.start, r.end)

object ArbRange extends ArbRange
