// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.Order
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.geom
import lucuma.core.geom.ShapeExpression
import lucuma.core.util.Enumerated

// Type for instruments or configurations that can only use pwfs1/pwfs2
sealed trait PWFSProbe
type PWFSGuideProbe = GuideProbe & PWFSProbe

/**
 * Enumerated type for guide probe
 */
enum GuideProbe(val tag: String) derives Enumerated:
  case PWFS1           extends GuideProbe("Pwfs1") with PWFSProbe
  case PWFS2           extends GuideProbe("Pwfs2") with PWFSProbe
  case GmosOIWFS       extends GuideProbe("GmosOiwfs")
  case Flamingos2OIWFS extends GuideProbe("Flamingos2Oiwfs")
  case AltairAOWFS     extends GuideProbe("AltairAowfs")

  def candidatesArea: ShapeExpression = this match
    // For pwfs1/2 417"
    case _: PWFSProbe    => geom.pwfs.patrolField.patrolField // for pwf1 the patrol field is the candidates area
    // For gmos oiwfs 294"
    case GmosOIWFS       => geom.gmos.candidatesArea.candidatesArea
    // For f2 oiwfs 222"
    case Flamingos2OIWFS => geom.flamingos2.candidatesArea.candidatesArea(Flamingos2LyotWheel.F16)
    // For Altair 54", the patrol field oval at any position angle
    case AltairAOWFS     => geom.altair.patrolField.candidatesArea

object GuideProbe:
  given Enumerated[PWFSGuideProbe] = Enumerated.from(GuideProbe.PWFS1, GuideProbe.PWFS2).withTag(_.tag)

  /**
   * Probes ranked best-first: the Altair AOWFS beats an OIWFS, which beats PWFS2,
   * which beats PWFS1. Every probe gets its own rank so the order is total and
   * usable in a SortedSet.
   */
  val Preference: Order[GuideProbe] =
    Order.by:
      case AltairAOWFS     => 0
      case GmosOIWFS       => 1
      case Flamingos2OIWFS => 2
      case PWFS2           => 3
      case PWFS1           => 4
