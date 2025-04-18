// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.derived.*
import lucuma.core.enums.ScienceBand

/**
 * Pairs an optional science band with a categorized time.  Because programs
 * may have observations in distinct bands, at the top-level we must group by
 * band.
 */
case class BandedTime(
  band: Option[ScienceBand],
  time: CategorizedTime
) derives Order

object BandedTime:
  val Empty: BandedTime = BandedTime(None, CategorizedTime.Zero)