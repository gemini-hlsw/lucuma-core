// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration unit arc lamps.
 * @group Enumerations
 */
enum GcalArc(val tag: String, val shortName: String, val longName: String) derives Enumerated:
  /** @group Constructors */ case ArArc extends GcalArc("ArArc", "Ar arc", "Ar arc")
  /** @group Constructors */ case ThArArc extends GcalArc("ThArArc", "ThAr arc", "ThAr arc")
  /** @group Constructors */ case CuArArc extends GcalArc("CuArArc", "CuAr arc", "CuAr arc")
  /** @group Constructors */ case XeArc extends GcalArc("XeArc", "Xe arc", "Xe arc")
