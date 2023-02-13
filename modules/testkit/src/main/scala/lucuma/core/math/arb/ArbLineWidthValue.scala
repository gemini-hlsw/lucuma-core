// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import eu.timepit.refined.auto.*
import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.arb.*
import lucuma.core.math.LineWidthValue
import lucuma.core.math.arb.ArbRefined.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbLineWidthValue:
  given Arbitrary[LineWidthValue] = Arbitrary(
    arbitrary[PosBigDecimal].map(_ % 1_000_000).map( bd =>
      bd.signum match // Open on 0, closed on 1e6, so we convert zeros to 1e6.
        case 0 => LineWidthValue.unsafeFrom(1_000_000)
        case _ => LineWidthValue.unsafeFrom(bd)
    )
  )

  given Cogen[LineWidthValue] = newTypeCogen(LineWidthValue)

object ArbLineWidthValue extends ArbLineWidthValue