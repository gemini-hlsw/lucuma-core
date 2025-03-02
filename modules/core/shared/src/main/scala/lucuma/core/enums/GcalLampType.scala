// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration lamp type.
 * @group Enumerations (Generated)
 */
enum GcalLampType(val tag: String) derives Enumerated:
  /** @group Constructors */ case Arc extends GcalLampType("Arc")
  /** @group Constructors */ case Flat extends GcalLampType("Flat")
