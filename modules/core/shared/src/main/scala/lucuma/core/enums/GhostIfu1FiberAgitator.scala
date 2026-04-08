// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum GhostIfu1FiberAgitator(val tag: String) derives Enumerated:
  case Disabled extends GhostIfu1FiberAgitator("disabled")
  case Enabled  extends GhostIfu1FiberAgitator("enabled")