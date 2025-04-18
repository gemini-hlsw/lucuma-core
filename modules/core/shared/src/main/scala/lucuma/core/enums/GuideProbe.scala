// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for guide probe
 */
enum GuideProbe(val tag: String) derives Enumerated:
  case PWFS1     extends GuideProbe("Pwfs1")
  case PWFS2     extends GuideProbe("Pwfs2")
  case GmosOIWFS extends GuideProbe("GmosOiwfs")
  case F2OIWFS   extends GuideProbe("Flamingos2Oiwfs")
