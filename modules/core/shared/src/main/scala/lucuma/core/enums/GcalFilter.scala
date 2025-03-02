// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration unit filter.
 * @group Enumerations
 */
enum GcalFilter(val tag: String, val shortName: String, val longName: String, val obsolete: Boolean) derives Enumerated:
  /** @group Constructors */ case None extends GcalFilter("None", "none", "none", false)
  /** @group Constructors */ case Gmos extends GcalFilter("Gmos", "GMOS balance", "GMOS balance", false)
  /** @group Constructors */ case Hros extends GcalFilter("Hros", "HROS balance", "HROS balance", true)
  /** @group Constructors */ case Nir extends GcalFilter("Nir", "NIR balance", "NIR balance", false)
  /** @group Constructors */ case Nd10 extends GcalFilter("Nd10", "ND1.0", "ND1.0", false)
  /** @group Constructors */ case Nd16 extends GcalFilter("Nd16", "ND1.6", "ND1.6", true)
  /** @group Constructors */ case Nd20 extends GcalFilter("Nd20", "ND2.0", "ND2.0", false)
  /** @group Constructors */ case Nd30 extends GcalFilter("Nd30", "ND3.0", "ND3.0", false)
  /** @group Constructors */ case Nd40 extends GcalFilter("Nd40", "ND4.0", "ND4.0", false)
  /** @group Constructors */ case Nd45 extends GcalFilter("Nd45", "ND4-5", "ND4-5", false)
  /** @group Constructors */ case Nd50 extends GcalFilter("Nd50", "ND5.0", "ND5.0", true)
