// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum SubaruInstrument(val tag: String) derives Enumerated:
  case Focas   extends SubaruInstrument("focas")
  case Hds     extends SubaruInstrument("hds")
  case Hsc     extends SubaruInstrument("hsc")
  case Ircs    extends SubaruInstrument("ircs")
  case Moircs  extends SubaruInstrument("moircs")
  case Pfs     extends SubaruInstrument("pfs")
  case Visitor extends SubaruInstrument("visitor")
