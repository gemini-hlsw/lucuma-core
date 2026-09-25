// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.numeric.PosBigDecimal
import org.scalacheck.*

trait LimitedBigDecimals:
  // Scala.js seems to have trouble formatting BigDecimals with very high absolute scale or precision.
  // Values are built within bounds instead of filtered, since filtering the default arbitrary
  // discards most samples and makes property checks give up.
  private def limitedBigDecimal(minUnscaled: Long): Gen[BigDecimal] =
    for
      digits   <- Gen.choose(1, 15)
      max       = BigInt(10).pow(digits).toLong - 1
      unscaled <- Gen.choose(minUnscaled.max(-max), max)
      scale    <- Gen.choose(-99, 99)
    yield BigDecimal(BigInt(unscaled), scale)

  given Arbitrary[BigDecimal] =
    Arbitrary(limitedBigDecimal(Long.MinValue))

  given Arbitrary[PosBigDecimal] =
    Arbitrary(limitedBigDecimal(1L).map(PosBigDecimal.unsafeFrom))

  given Arbitrary[NonNegBigDecimal] =
    Arbitrary(limitedBigDecimal(0L).map(NonNegBigDecimal.unsafeFrom))
