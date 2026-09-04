// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type of expected conditions.
 */
enum ConditionsExpectationType(val tag: String, val name: String) derives Enumerated:
  case ClearSkies  extends ConditionsExpectationType("clear_skies",  "Clear Skies")
  case Fog         extends ConditionsExpectationType("fog",          "Fog")
  case ThickClouds extends ConditionsExpectationType("thick_clouds", "Thick Clouds")
  case ThinClouds  extends ConditionsExpectationType("thin_clouds",  "Thin Clouds")
