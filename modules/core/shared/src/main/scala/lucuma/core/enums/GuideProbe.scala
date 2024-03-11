// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for guide probe
 */
enum GuideProbe(val tag: String) extends Product with Serializable derives Enumerated {
  case PWFS1     extends GuideProbe("Pwfs2")
  case PWFS2     extends GuideProbe("Pwfs1")
  case GmosOIWFS extends GuideProbe("GmosOiwfs")
}
