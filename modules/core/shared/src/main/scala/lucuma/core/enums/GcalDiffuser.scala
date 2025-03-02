// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration unit diffusers.
 * @group Enumerations
 */
enum GcalDiffuser(val tag: String, val shortName: String, val longName: String, val obsolete: Boolean) derives Enumerated:
  /** @group Constructors */ case Ir extends GcalDiffuser("Ir", "IR", "IR", false)
  /** @group Constructors */ case Visible extends GcalDiffuser("Visible", "Visible", "Visible", false)
