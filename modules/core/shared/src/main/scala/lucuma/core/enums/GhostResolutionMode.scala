// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum GhostResolutionMode(val tag: String, val shortName: String, val longName: String) derives Enumerated:
  case Standard extends GhostResolutionMode("standard", "Standard", "Standard Resolution")
  case High     extends GhostResolutionMode("high", "High", "High Resolution")
