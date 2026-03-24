// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.geom
import lucuma.core.geom.ShapeExpression
import lucuma.core.util.Enumerated

/**
 * Enumerated type for guide probe
 */
enum GuideProbe(val tag: String) derives Enumerated:
  case PWFS1           extends GuideProbe("Pwfs1")
  case PWFS2           extends GuideProbe("Pwfs2")
  case GmosOIWFS       extends GuideProbe("GmosOiwfs")
  case Flamingos2OIWFS extends GuideProbe("Flamingos2Oiwfs")

  def candidatesArea: ShapeExpression = this match
    // For pwfs1/2 417"
    case PWFS1 | PWFS2   => geom.pwfs.patrolField.patrolField // for pwf1 the patrol field is the candidates area
    // For gmos oiwfs 294"
    case GmosOIWFS       => geom.gmos.candidatesArea.candidatesArea
    // For f2 oiwfs 222"
    case Flamingos2OIWFS => geom.flamingos2.candidatesArea.candidatesArea(Flamingos2LyotWheel.F16)
