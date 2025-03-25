// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.numeric.PosBigDecimal
import org.scalacheck.*

trait LimitedBigDecimals:
  // Scala.js seems to have trouble formatting BigDecimals with very high absolute scale or precision.
  // We therefore use these bounded arbitraries.
  given Arbitrary[BigDecimal] =
    Arbitrary(
      org.scalacheck.Arbitrary.arbBigDecimal.arbitrary.suchThat(x =>
        x.scale.abs < 100 && x.precision <= 15
      )
    )

  given Arbitrary[PosBigDecimal] =
    Arbitrary(
      given_Arbitrary_BigDecimal.arbitrary
        .map(_.abs)
        .suchThat(_ > 0)
        .map(PosBigDecimal.unsafeFrom)
    )

  given Arbitrary[NonNegBigDecimal] =
    Arbitrary(
      given_Arbitrary_BigDecimal.arbitrary
        .map(_.abs)
        .suchThat(_ >= 0)
        .map(NonNegBigDecimal.unsafeFrom)
    )
