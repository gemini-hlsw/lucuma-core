// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.model.Percentile
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/** Percentiles from: https://www.gemini.edu/observing/telescopes-and-sites/sites#SkyTransparencyWater */
enum WaterVapor(val tag: String, val label: String, val percentile: Percentile) derives Enumerated:
  case VeryDry extends WaterVapor("very_dry", "Very Dry", Percentile.unsafeFromPercent(20))
  case Dry     extends WaterVapor("dry", "Dry", Percentile.unsafeFromPercent(50))
  case Median  extends WaterVapor("median", "Median", Percentile.unsafeFromPercent(80))
  case Wet     extends WaterVapor("wet", "Wet",  Percentile.unsafeFromPercent(100))

object WaterVapor:
  given Display[WaterVapor] = Display.byShortName(_.label)
