// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import lucuma.core.math.arb.ArbOffset
import lucuma.core.model.sequence.ABBA
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen

trait ArbABBA:

  import ArbOffset.given

  given Arbitrary[ABBA] = Arbitrary:
    for
      a <- Arbitrary.arbitrary[lucuma.core.math.Offset]
      b <- Arbitrary.arbitrary[lucuma.core.math.Offset]
    yield ABBA(a, b)

  given Cogen[ABBA] =
    Cogen[(lucuma.core.math.Offset, lucuma.core.math.Offset)].contramap(abba => (abba.a, abba.b))

object ArbABBA extends ArbABBA