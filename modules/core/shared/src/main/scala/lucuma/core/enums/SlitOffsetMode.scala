// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Slit Offset Mode.
 */
enum SlitOffsetMode(
  val tag: String,
  val description: String
) derives Enumerated, Display:
  case NodAlongSlit extends SlitOffsetMode("nod_along_slit", "Nod along slit")
  case NodToSky     extends SlitOffsetMode("nod_to_sky",     "Nod to sky")
