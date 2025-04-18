// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import lucuma.core.util.TimeSpan

/**
 * Setup time is an estimate for slewing, configuring, and acquiring the
 * observation target.  There are estimates for a full setup from scratch
 * and an alternative reacquisition time which doesn't require as much
 * acquisition time.
 */
case class SetupTime(
  full:          TimeSpan,
  reacquisition: TimeSpan
)

object SetupTime {

  val Zero: SetupTime =
    SetupTime(
      TimeSpan.Zero,
      TimeSpan.Zero
    )

  given Eq[SetupTime] =
    Eq.by { a => (
      a.full,
      a.reacquisition
    )}

}