// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum SpectroscopyCapability(val tag: String, val label: String) derives Enumerated:
  case NodAndShuffle extends SpectroscopyCapability("nod_and_shuffle", "nod and shuffle")
  case Polarimetry   extends SpectroscopyCapability("polarimetry",     "polarimetry")
  case Coronagraphy  extends SpectroscopyCapability("coronagraphy",    "coronagraphy")
