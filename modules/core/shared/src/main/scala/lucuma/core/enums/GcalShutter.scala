// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration unit shutter states.
 * @group Enumerations
 */
enum GcalShutter(val tag: String, val shortName: String, val longName: String, val obsolete: Boolean) derives Enumerated:
  /** @group Constructors */ case Open extends GcalShutter("Open", "Open", "Open", false)
  /** @group Constructors */ case Closed extends GcalShutter("Closed", "Closed", "Closed", false)
