// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration unit arc lamps.
 * @group Enumerations
 */
enum GcalArc(val tag: String, val shortName: String, val longName: String, val obsolete: Boolean) derives Enumerated:
  /** @group Constructors */ case ArArc extends GcalArc("ArArc", "Ar arc", "Ar arc", false)
  /** @group Constructors */ case ThArArc extends GcalArc("ThArArc", "ThAr arc", "ThAr arc", false)
  /** @group Constructors */ case CuArArc extends GcalArc("CuArArc", "CuAr arc", "CuAr arc", false)
  /** @group Constructors */ case XeArc extends GcalArc("XeArc", "Xe arc", "Xe arc", false)
