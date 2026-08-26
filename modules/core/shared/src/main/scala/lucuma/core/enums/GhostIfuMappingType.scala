// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum GhostIfuMappingType(val tag: String, val name: String) derives Enumerated:
  case SingleTarget  extends GhostIfuMappingType("single_target",   "Single Target")
  case TargetPlusSky extends GhostIfuMappingType("target_plus_sky", "Target+Sky")
  case SkyPlusTarget extends GhostIfuMappingType("sky_plus_target", "Sky+Target")
  case DualTarget    extends GhostIfuMappingType("dual_target",     "Dual Target")