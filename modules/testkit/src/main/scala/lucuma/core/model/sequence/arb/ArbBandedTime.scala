// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.ScienceBand
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbBandedTime:
  import ArbCategorizedTime.given
  import ArbEnumerated.given

  given Arbitrary[BandedTime] =
    Arbitrary {
      arbitrary[(Option[ScienceBand], CategorizedTime)].map(BandedTime.apply)
    }

  given Cogen[BandedTime] =
    Cogen[(Option[ScienceBand], CategorizedTime)].contramap(b => (b.band, b.time))

object ArbBandedTime extends ArbBandedTime