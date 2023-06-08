// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for "smart" calibration sequence types.
 */
enum SmartGcalType(val tag: String, val name: String) derives Enumerated {

  case Arc           extends SmartGcalType("arc",            "Arc")
  case Flat          extends SmartGcalType("flat",           "Flat")
  case DayBaseline   extends SmartGcalType("day_baseline",   "Day Baseline")
  case NightBaseline extends SmartGcalType("night_baseline", "Night Baseline")

  def fold[X](lamp: GcalLampType => X, baseline: GcalBaselineType => X): X =
    this match {
      case Arc           => lamp(GcalLampType.Arc)
      case Flat          => lamp(GcalLampType.Flat)
      case DayBaseline   => baseline(GcalBaselineType.Day)
      case NightBaseline => baseline(GcalBaselineType.Night)
    }

}