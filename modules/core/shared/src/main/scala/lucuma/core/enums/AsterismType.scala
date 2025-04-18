// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for asterism types.
 * @group Enumerations
 */
enum AsterismType(val tag: String) derives Enumerated:
  /** @group Constructors */ case GhostDualTarget extends AsterismType("GhostDualTarget")
  /** @group Constructors */ case SingleTarget extends AsterismType("SingleTarget")
