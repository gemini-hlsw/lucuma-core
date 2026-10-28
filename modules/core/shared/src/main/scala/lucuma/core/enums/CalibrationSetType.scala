// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * The kind of exposure a daytime calibration set produces.
 */
enum CalibrationSetType(val tag: String, val name: String) derives Enumerated:
  case Bias           extends CalibrationSetType("bias",            "Bias")
  case Dark           extends CalibrationSetType("dark",            "Dark")
  case Flat           extends CalibrationSetType("flat",            "Flat")
  case Arc            extends CalibrationSetType("arc",             "Arc")
  case DaytimeObserve extends CalibrationSetType("daytime_observe", "Observe")
  case SmartFlat      extends CalibrationSetType("smart_flat",      "Smart Flat")
  case SmartArc       extends CalibrationSetType("smart_arc",       "Smart Arc")
  case DayBaseline    extends CalibrationSetType("day_baseline",    "Day Baseline")

  /**
   * Whether this kind is taken with a GCAL lamp.  A manual flat or arc names
   * its lamp configuration; a smart kind gets it from the Smart GCAL lookup.
   */
  def usesGcal: Boolean =
    this match
      case Flat | Arc | SmartFlat | SmartArc | DayBaseline => true
      case Bias | Dark | DaytimeObserve                    => false

  /**
   * The Smart GCAL lookup that resolves this kind, if it is a smart kind.  A
   * smart kind takes its lamp configuration, exposure time and exposure count
   * from the lookup rather than from the row.
   */
  def smartGcalType: Option[SmartGcalType] =
    this match
      case SmartFlat   => Some(SmartGcalType.Flat)
      case SmartArc    => Some(SmartGcalType.Arc)
      case DayBaseline => Some(SmartGcalType.DayBaseline)
      case _           => None

  def isSmart: Boolean =
    smartGcalType.isDefined
