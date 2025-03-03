// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration unit continuum lamps.
 * @group Enumerations
 */
enum GcalContinuum(val tag: String, val shortName: String, val longName: String) derives Enumerated:
  /** @group Constructors */ case IrGreyBodyLow     extends GcalContinuum("IrGreyBodyLow", "IR grey body - low", "IR grey body - low")
  /** @group Constructors */ case IrGreyBodyHigh    extends GcalContinuum("IrGreyBodyHigh", "IR grey body - high", "IR grey body - high")
  /** @group Constructors */ case QuartzHalogen5W   extends GcalContinuum("QuartzHalogen5", "5W Quartz Halogen", "5W Quartz Halogen")
  /** @group Constructors */ case QuartzHalogen100W extends GcalContinuum("QuartzHalogen100", "100W Quartz Halogen", "100W Quartz Halogen")
