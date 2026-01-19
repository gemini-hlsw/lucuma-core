// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.all.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TrackType

trait probes:
  def guideProbe(observingMode: ObservingModeType, trackType: TrackType): Option[GuideProbe] =
    (observingMode, trackType) match
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

object probes extends probes
