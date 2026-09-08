// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.all.*
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.FacilityObservingModeType
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PWFSGuideProbe
import lucuma.core.enums.TrackType
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.util.Enumerated

import scala.collection.immutable.SortedSet

trait probes:

  private given Ordering[GuideProbe] = GuideProbe.Preference.toOrdering

  private val Pwfs: SortedSet[GuideProbe] =
    SortedSet.from(Enumerated[PWFSGuideProbe].all)

  /**
   * Probes AGS has geometry for in the given observing mode, i.e. the probes a
   * user may legitimately pick when overriding the automatic choice made by
   * `guideProbe`.  Ordered best-first, see `GuideProbe.Preference`.
   */
  def allowedProbes(observingMode: ObservingModeType): SortedSet[GuideProbe] =
    observingMode match
      // Exchange observations are not supported by AGS; there is no guide probe.
      case _: ExchangeObservingModeType => SortedSet.empty
      case _: VisitorObservingModeType  => Pwfs
      case m: FacilityObservingModeType => facilityProbes(m)

  def isProbeAllowed(observingMode: ObservingModeType, probe: GuideProbe): Boolean =
    allowedProbes(observingMode).contains(probe)

  // Split out so the compiler enforces exhaustivity: adding a facility mode
  // must not silently fall through to a MatchError at runtime.
  private def facilityProbes(observingMode: FacilityObservingModeType): SortedSet[GuideProbe] =
    observingMode match
      case FacilityObservingModeType.GmosNorthLongSlit | FacilityObservingModeType.GmosSouthLongSlit |
           FacilityObservingModeType.GmosNorthImaging  | FacilityObservingModeType.GmosSouthImaging  |
           FacilityObservingModeType.GmosNorthMos      | FacilityObservingModeType.GmosSouthMos      |
           FacilityObservingModeType.GmosNorthIfu      | FacilityObservingModeType.GmosSouthIfu      =>
        Pwfs + GuideProbe.GmosOIWFS
      case FacilityObservingModeType.Flamingos2LongSlit | FacilityObservingModeType.Flamingos2Imaging |
           FacilityObservingModeType.Flamingos2Mos =>
        Pwfs + GuideProbe.Flamingos2OIWFS
      case FacilityObservingModeType.Igrins2LongSlit =>
        Pwfs
      case FacilityObservingModeType.GnirsImaging | FacilityObservingModeType.GnirsLongSlit |
           FacilityObservingModeType.GnirsIfu =>
        Pwfs
      // PWFS1 vignettes the GHOST field.
      case FacilityObservingModeType.GhostIfu =>
        SortedSet(GuideProbe.PWFS2)

  /**
   * The probe AGS selects by default: the most preferred allowed probe.  An
   * OIWFS cannot track a nonsidereal target, so it drops out of the running.
   */
  def guideProbe(observingMode: ObservingModeType, trackType: TrackType): Option[GuideProbe] =
    allowedProbes(observingMode)
      .filter:
        case GuideProbe.GmosOIWFS | GuideProbe.Flamingos2OIWFS => trackType === TrackType.Sidereal
        case _                                                 => true
      .headOption

object probes extends probes