// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum SubaruInstrument(val tag: String, val name: String) derives Enumerated, Display:
  case Focas   extends SubaruInstrument("focas", "FOCAS")
  case Hds     extends SubaruInstrument("hds", "HDS")
  case Hsc     extends SubaruInstrument("hsc", "HSC")
  case Ircs    extends SubaruInstrument("ircs", "IRCS")
  case Moircs  extends SubaruInstrument("moircs", "MOIRCS")
  case Pfs     extends SubaruInstrument("pfs", "PFS")
  case Visitor extends SubaruInstrument("visitor", "Visitor")
