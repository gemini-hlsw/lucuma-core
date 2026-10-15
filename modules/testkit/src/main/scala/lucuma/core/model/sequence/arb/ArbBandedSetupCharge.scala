// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.ScienceBand
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbBandedSetupCharge:
  import ArbSetupCharge.given
  import ArbEnumerated.given

  given Arbitrary[BandedSetupCharge] =
    Arbitrary:
      arbitrary[(Option[ScienceBand], SetupCharge)].map(BandedSetupCharge.apply)

  given Cogen[BandedSetupCharge] =
    Cogen[(Option[ScienceBand], SetupCharge)].contramap(b => (b.band, b.charge))

object ArbBandedSetupCharge extends ArbBandedSetupCharge
