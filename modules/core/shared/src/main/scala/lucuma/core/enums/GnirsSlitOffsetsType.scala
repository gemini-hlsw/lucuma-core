// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum GnirsSlitOffsetsType(val tag: String, val shortName: String, val longName: String) derives Enumerated, Display:
  case OnSlit extends GnirsSlitOffsetsType("OnSlit", "On-slit", "ABBA on-slit")
  case OffSlit extends GnirsSlitOffsetsType("OffSlit", "Off-slit", "ABBA off-slit")
