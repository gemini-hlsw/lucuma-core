// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.model.IntCentiPercent
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/** Percentiles from: https://www.gemini.edu/observing/telescopes-and-sites/sites#SkyTransparencyWater */
enum WaterVapor(val tag: String, val label: String, val percentile: IntCentiPercent) derives Enumerated:
  case VeryDry extends WaterVapor("very_dry", "Very Dry", IntCentiPercent.unsafeFromPercent(20))
  case Dry     extends WaterVapor("dry", "Dry", IntCentiPercent.unsafeFromPercent(50))
  case Median  extends WaterVapor("median", "Median", IntCentiPercent.unsafeFromPercent(80))
  case Wet     extends WaterVapor("wet", "Wet",  IntCentiPercent.unsafeFromPercent(100))

object WaterVapor:
  given Display[WaterVapor] = Display.byShortName(_.label)
