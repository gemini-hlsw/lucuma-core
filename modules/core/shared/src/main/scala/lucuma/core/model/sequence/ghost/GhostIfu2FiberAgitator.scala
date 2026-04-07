// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import lucuma.core.util.Enumerated

enum GhostIfu2FiberAgitator(val tag: String) derives Enumerated:
  case Disabled extends GhostIfu2FiberAgitator("disabled")
  case Enabled  extends GhostIfu2FiberAgitator("enabled")