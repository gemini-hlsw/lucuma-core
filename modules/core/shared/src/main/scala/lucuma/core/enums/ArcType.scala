// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for arc types.
 * @group Enumerations
 */
enum ArcType(val tag: String) derives Enumerated:
  /** @group Constructors */ case Empty extends ArcType("empty")
  /** @group Constructors */ case Full extends ArcType("full")
  /** @group Constructors */ case Partial extends ArcType("partial")
