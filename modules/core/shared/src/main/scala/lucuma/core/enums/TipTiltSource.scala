// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/** Enumerated type for Tip/Tilt Source. */
enum TipTiltSource(val tag: String) derives Enumerated {
  case PWFS1 extends TipTiltSource("Pwfs1")
  case PWFS2 extends TipTiltSource("Pwfs2")
  case OIWFS extends TipTiltSource("Oiwfs")
  case GAOS  extends TipTiltSource("Gaos")
}

