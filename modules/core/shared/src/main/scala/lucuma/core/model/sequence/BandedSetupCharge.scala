// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.derived.*
import lucuma.core.enums.ScienceBand

/**
 * Pairs an optional science band with a setup charge.  Because programs may
 * have observations in distinct bands, at the top-level we must group by band.
 */
case class BandedSetupCharge(
  band:   Option[ScienceBand],
  charge: SetupCharge
) derives Order

object BandedSetupCharge:
  val Empty: BandedSetupCharge = BandedSetupCharge(None, SetupCharge.Zero)
