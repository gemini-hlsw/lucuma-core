// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional.arb

import lucuma.core.math.dimensional.*
import lucuma.core.util.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbMeasure {
  import ArbUnits.given

  given arbMeasure[N: Arbitrary]: Arbitrary[Measure[N]] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[Units]
        e <- arbitrary[Option[N]]
      } yield u.withValue(n, e)
    }

  given cogenMeasure[N: Cogen]: Cogen[Measure[N]] =
    Cogen[(N, Units, Option[N])].contramap(m => (m.value, m.units, m.error))

  given arbTaggedMeasure[N: Arbitrary, Tag](using
    arbUnit: Arbitrary[Units Of Tag]
  ): Arbitrary[Measure[N] Of Tag] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[Units Of Tag]
        e <- arbitrary[Option[N]]
      } yield u.withValueTagged(n, e)
    }

  given cogenTaggedMeasure[N: Cogen, Tag]: Cogen[Measure[N] Of Tag] =
    Cogen[(N, Units, Option[N])].contramap(m => (m.value, m.units, m.error))
}

object ArbMeasure extends ArbMeasure
