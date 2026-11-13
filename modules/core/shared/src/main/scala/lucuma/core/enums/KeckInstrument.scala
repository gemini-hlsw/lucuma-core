// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum KeckInstrument(val tag: String, val name: String) derives Enumerated, Display:
  case Hires extends KeckInstrument("hires", "HIRES")
  case Other extends KeckInstrument("other", "Other")
