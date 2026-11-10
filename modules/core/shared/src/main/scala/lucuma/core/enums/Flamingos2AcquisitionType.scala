// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum Flamingos2AcquisitionType(val tag: String, val shortName: String, val longName: String) derives Enumerated, Display:
  case Bright extends Flamingos2AcquisitionType("Bright", "Bright", "Bright Target")
  case Faint extends Flamingos2AcquisitionType("Faint", "Faint", "Faint Target")
