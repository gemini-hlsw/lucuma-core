// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.all.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TrackType
import lucuma.core.enums.VisitorObservingModeType

trait probes:
  def guideProbe(observingMode: ObservingModeType, trackType: TrackType): Option[GuideProbe] =
    (observingMode, trackType) match
      case (ObservingModeType.Flamingos2LongSlit | ObservingModeType.Flamingos2Imaging, TrackType.Nonsidereal) =>
        GuideProbe.PWFS2.some
      case (ObservingModeType.Flamingos2LongSlit | ObservingModeType.Flamingos2Imaging, TrackType.Sidereal) =>
        GuideProbe.Flamingos2OIWFS.some
      // gmos modes could be condensed but I'll split them into longslit, imaging, and mos anyway
      case (ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit, TrackType.Nonsidereal) =>
        GuideProbe.PWFS2.some
      case (ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit, TrackType.Sidereal) =>
        GuideProbe.GmosOIWFS.some
      case (ObservingModeType.GmosNorthMos | ObservingModeType.GmosSouthMos, TrackType.Nonsidereal) =>
        GuideProbe.PWFS2.some
      case (ObservingModeType.GmosNorthMos | ObservingModeType.GmosSouthMos, TrackType.Sidereal) =>
        GuideProbe.GmosOIWFS.some
      case (ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging, TrackType.Nonsidereal) =>
        GuideProbe.PWFS2.some
      case (ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging, TrackType.Sidereal) =>
        GuideProbe.GmosOIWFS.some
      case (ObservingModeType.Igrins2LongSlit, _) =>
        GuideProbe.PWFS2.some
      case (ObservingModeType.GhostIfu, _) =>
        GuideProbe.PWFS2.some
      case (ObservingModeType.GnirsLongSlit | ObservingModeType.GnirsIfu, _) =>
        GuideProbe.PWFS2.some
      case (_: VisitorObservingModeType, _) =>
        GuideProbe.PWFS2.some

object probes extends probes
