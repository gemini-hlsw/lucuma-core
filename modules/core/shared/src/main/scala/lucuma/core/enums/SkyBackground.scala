// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.model.Percentile
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/** Percentiles from: https://www.gemini.edu/observing/telescopes-and-sites/sites#SkyBackground */
enum SkyBackground(val tag: String, val label: String, val percentile: Percentile) derives Enumerated:
  case Darkest extends SkyBackground("darkest", "Darkest", Percentile.unsafeFromPercent(20))
  case Dark    extends SkyBackground("dark", "Dark", Percentile.unsafeFromPercent(50)) 
  case Gray    extends SkyBackground("gray", "Gray", Percentile.unsafeFromPercent(80))
  case Bright  extends SkyBackground("bright", "Bright", Percentile.unsafeFromPercent(100))

object SkyBackground:
  given Display[SkyBackground] = Display.byShortName(_.label)
