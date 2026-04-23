// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.all.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TrackType

trait probes:
  val guideProbe: (ObservingModeType,  TrackType) => Option[GuideProbe] =
    case (ObservingModeType.Flamingos2LongSlit, TrackType.Nonsidereal) =>
      GuideProbe.PWFS2.some
    case (ObservingModeType.Flamingos2LongSlit, TrackType.Sidereal) =>
      GuideProbe.Flamingos2OIWFS.some
    // gmos modes could be condensed but I'll split them into longslit and imaging anyway
    case (ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit, TrackType.Nonsidereal) =>
      GuideProbe.PWFS2.some
    case (ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit, TrackType.Sidereal) =>
      GuideProbe.GmosOIWFS.some
    case (ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging, TrackType.Nonsidereal) =>
      GuideProbe.PWFS2.some
    case (ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging, TrackType.Sidereal) =>
      GuideProbe.GmosOIWFS.some
    case (ObservingModeType.Igrins2LongSlit, _) =>
      GuideProbe.PWFS2.some
    case (ObservingModeType.GhostIfu, _) =>
      GuideProbe.PWFS2.some
    case (ObservingModeType.GnirsLongSlit, _) =>
      GuideProbe.PWFS2.some

object probes extends probes
