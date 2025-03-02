// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS ADC.
 * @group Enumerations (Generated)
 */
enum GmosAdc(val tag: String, val shortName: String, val longName: String) derives Enumerated:
  /** @group Constructors */ case BestStatic extends GmosAdc("BestStatic", "Best Static", "Best Static Correction")
  /** @group Constructors */ case Follow extends GmosAdc("Follow", "Follow", "Follow During Exposure")
