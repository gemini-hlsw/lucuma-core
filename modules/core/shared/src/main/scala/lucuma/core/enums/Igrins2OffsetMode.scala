// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for IGRINS-2 Offset Mode.
 */
enum Igrins2OffsetMode(
  val tag: String,
  val description: String
) derives Enumerated:

  case NodAlongSlit extends Igrins2OffsetMode("nod_along_slit", "Nod along slit")
  case NodToSky     extends Igrins2OffsetMode("nod_to_sky",     "Nod to sky")
