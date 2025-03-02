// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum GalaxySpectrum(val tag: String, val name: String) derives Enumerated:
  case Elliptical extends GalaxySpectrum("Elliptical", "Elliptical (22nm - 9.7μm)")
  case Spiral     extends GalaxySpectrum("Spiral", "Spiral (Sc, 22nm - 9.7μm)")

object GalaxySpectrum:
  given Display[GalaxySpectrum] = Display.byShortName(_.name)
