// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Altair neutral density filter, placed in front of the wavefront sensor for very bright natural
 * guide stars. It only attenuates the Altair path, never the science beam.
 */
enum AltairNdFilter(val tag: String, val name: String) derives Enumerated, Display:
  case In  extends AltairNdFilter("in", "In")
  case Out extends AltairNdFilter("out", "Out")
