// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration unit filter.
 * @group Enumerations
 */
enum GcalFilter(val tag: String, val shortName: String, val longName: String) derives Enumerated:
  /** @group Constructors */ case None extends GcalFilter("None", "none", "none")
  /** @group Constructors */ case Gmos extends GcalFilter("Gmos", "GMOS balance", "GMOS balance")
  /** @group Constructors */ case Nir extends GcalFilter("Nir", "NIR balance", "NIR balance")
  /** @group Constructors */ case Nd10 extends GcalFilter("Nd10", "ND1.0", "ND1.0")
  /** @group Constructors */ case Nd20 extends GcalFilter("Nd20", "ND2.0", "ND2.0")
  /** @group Constructors */ case Nd30 extends GcalFilter("Nd30", "ND3.0", "ND3.0")
  /** @group Constructors */ case Nd40 extends GcalFilter("Nd40", "ND4.0", "ND4.0")
  /** @group Constructors */ case Nd45 extends GcalFilter("Nd45", "ND4-5", "ND4-5")