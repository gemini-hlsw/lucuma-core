// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum SkyBackground(val tag: String, val label: String) derives Enumerated:
  case Darkest extends SkyBackground("darkest", "Darkest")
  case Dark    extends SkyBackground("dark", "Dark") 
  case Gray    extends SkyBackground("gray", "Gray")
  case Bright  extends SkyBackground("bright", "Bright")

object SkyBackground:
  given Display[SkyBackground] = Display.byShortName(_.label)
