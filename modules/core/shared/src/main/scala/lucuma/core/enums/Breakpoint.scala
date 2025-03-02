// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum Breakpoint(val tag: String) derives Enumerated:
  /** @group Constructors */ case Enabled extends Breakpoint("enabled")
  /** @group Constructors */ case Disabled extends Breakpoint("disabled")
