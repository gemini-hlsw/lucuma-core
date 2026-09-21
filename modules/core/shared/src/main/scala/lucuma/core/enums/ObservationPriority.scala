// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Relative priority assigned to an observation.
 *
 * Declared in ascending order so the derived `Order` runs `Low` < `Medium` < `High`.
 *
 * @group Enumerations
 */
enum ObservationPriority(
  val tag:         String,
  val description: String
) derives Enumerated,
      Display:
  case Low    extends ObservationPriority("low",    "Low")
  case Medium extends ObservationPriority("medium", "Medium")
  case High   extends ObservationPriority("high",   "High")

object ObservationPriority:
  val Default: ObservationPriority = Medium
