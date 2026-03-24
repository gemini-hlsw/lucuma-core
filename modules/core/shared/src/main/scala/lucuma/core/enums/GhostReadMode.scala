// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

// In OCS this is GhostReadNoiseGain and includes a gain. But the gain is always low so we removed it here. (high was deprecated)
enum GhostReadMode(val tag: String, val shortName: String, val longName: String, val readRate: Int) derives Enumerated:
  case Slow  extends GhostReadMode("slow",  "Slow",  "Slow Readout", 10)
  case Medium extends GhostReadMode("medium", "Medium", "Medium Readout", 5)
  case Fast   extends GhostReadMode("fast",  "Fast",  "Fast Readout", 2)

object GhostReadMode:
  val DefaultBlue: GhostReadMode = GhostReadMode.Slow
  val DefaultRed:  GhostReadMode = GhostReadMode.Medium  
