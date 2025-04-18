// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS amp count.
 * @group Enumerations (Generated)
 */
enum GmosAmpCount(val tag: String, val shortName: String, val longName: String) derives Enumerated:
  /** @group Constructors */ case Three extends GmosAmpCount("Three", "Three", "Three")
  /** @group Constructors */ case Six extends GmosAmpCount("Six", "Six", "Six")
  /** @group Constructors */ case Twelve extends GmosAmpCount("Twelve", "Twelve", "Twelve")
