// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum WaterVapor(val tag: String, val label: String) derives Enumerated:
  case VeryDry extends WaterVapor("very_dry", "Very Dry")
  case Dry     extends WaterVapor("dry", "Dry")
  case Median  extends WaterVapor("median", "Median")
  case Wet     extends WaterVapor("wet", "Wet")

object WaterVapor:
  given Display[WaterVapor] = Display.byShortName(_.label)
