// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.exposure

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SkyBackground
import lucuma.core.model.sequence.exposure.ExposureTimeViolation.Severity
import lucuma.core.model.sequence.ghost.MaxExposureTime as GhostMaxExposureTime
import lucuma.core.model.sequence.ghost.MaxWarningExposureTime as GhostMaxWarningExposureTime
import lucuma.core.model.sequence.ghost.MinExposureTime as GhostMinExposureTime
import lucuma.core.model.sequence.gmos.BackgroundSaturation
import lucuma.core.model.sequence.gmos.MaxWarningExposureTime as GmosMaxWarningExposureTime
import lucuma.core.model.sequence.gmos.MinExposureTime as GmosMinExposureTime
import lucuma.core.model.sequence.igrins2.MaxExposureTime as Igrins2MaxExposureTime
import lucuma.core.model.sequence.igrins2.MinExposureTime as Igrins2MinExposureTime
import lucuma.core.model.sequence.igrins2.SvcMinExposureTime as Igrins2SvcMinExposureTime
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

/**
 * The exposure time rules for each instrument, as functions of just the
 * configuration they depend on.  `PendingExposureRules` applies them to
 * sequence steps; an editor can call them directly with what it knows, before
 * there is a step to check.
 */
object ExposureRules:

  val CosmicRays: NonEmptyString =
    nes("due to cosmic ray contamination")

  val ReadNoise: NonEmptyString =
    nes("where a lower read noise mode is normally used")

  val SkySaturation: NonEmptyString =
    nes("due to sky background saturation")

  private def nes(s: String): NonEmptyString =
    NonEmptyString.unsafeFrom(s)

  // Flamingos 2

  private val flamingos2Rules: Map[Flamingos2ReadMode, List[ExposureRule]] =
    Enumerated[Flamingos2ReadMode].all.fproduct: readMode =>
      val subject = nes(s"Flamingos 2 ${readMode.shortName.toLowerCase} read mode")
      List(
        ExposureRule.Limits(subject, none, Severity.Error,   ExposureTimeLimits.fromMin(readMode.minimumExposureTime)),
        ExposureRule.WholeSeconds(nes("Flamingos 2")),
        ExposureRule.Limits(subject, none, Severity.Warning, ExposureTimeLimits.fromMin(readMode.recommendedExposureTime))
      )
    .toMap

  def flamingos2(readMode: Flamingos2ReadMode): List[ExposureRule] =
    flamingos2Rules(readMode)

  // GHOST, the same for each camera

  private def ghostCamera(camera: String): List[ExposureRule] =
    val subject = nes(s"the GHOST $camera camera")
    List(
      ExposureRule.Limits(subject, none,            Severity.Error,   ExposureTimeLimits.unsafeFromMinMax(GhostMinExposureTime, GhostMaxExposureTime)),
      ExposureRule.Limits(subject, CosmicRays.some, Severity.Warning, ExposureTimeLimits.fromMax(GhostMaxWarningExposureTime))
    )

  val ghostRed: List[ExposureRule]  = ghostCamera("red")
  val ghostBlue: List[ExposureRule] = ghostCamera("blue")

  // GMOS

  /**
   * What the GMOS sky background saturation check needs to know about an
   * imaging exposure.  Only low gain science imaging is checked; as in OCS,
   * acquisitions are not.
   */
  final case class GmosImaging[F](
    observeClass: ObserveClass,
    filter:       Option[F],
    xBin:         GmosXBinning,
    yBin:         GmosYBinning,
    ampGain:      GmosAmpGain
  )

  // The rules for every GMOS exposure, the same for each step and so built
  // once per site.
  private def gmosBaseRules(instrument: NonEmptyString): List[ExposureRule] =
    List(
      ExposureRule.Limits(instrument, none,            Severity.Error,   ExposureTimeLimits.fromMin(GmosMinExposureTime)),
      ExposureRule.WholeSeconds(instrument),
      ExposureRule.Limits(instrument, CosmicRays.some, Severity.Warning, ExposureTimeLimits.fromMax(GmosMaxWarningExposureTime))
    )

  private val GmosNorthName: NonEmptyString     = nes("GMOS North")
  private val gmosNorthBase: List[ExposureRule] = gmosBaseRules(GmosNorthName)

  private val GmosSouthName: NonEmptyString     = nes("GMOS South")
  private val gmosSouthBase: List[ExposureRule] = gmosBaseRules(GmosSouthName)

  // The sky background saturation rules for an imaging exposure, which depend
  // on the step.  GMOS North and South differ only in their filters and
  // saturation times.  The saturation time is for unbinned pixels; a binned
  // pixel collects background from xBin × yBin of them.  Exposures past half
  // the saturation time merit a warning.
  private def gmosSkyRules[F](
    instrument: NonEmptyString,
    saturation: Map[F, BackgroundSaturation.Row],
    filterName: F => String,
    imaging:    Option[GmosImaging[F]],
    sky:        SkyBackground
  ): List[ExposureRule] =
    (for
      img     <- imaging
      if img.observeClass === ObserveClass.Science && img.ampGain === GmosAmpGain.Low
      f       <- img.filter
      row     <- saturation.get(f)
      sat     <- row.get(sky)
      x        = img.xBin.value.count.value.toLong
      y        = img.yBin.value.count.value.toLong
      limit    = sat.toMicroseconds / (x * y)
      subject  = nes(s"$instrument ${filterName(f)} imaging with ${x}x$y binning under a ${sky.label} sky")
    yield List(
      ExposureRule.Limits(subject, SkySaturation.some, Severity.Error,   ExposureTimeLimits.fromMax(TimeSpan.unsafeFromMicroseconds(limit))),
      ExposureRule.Limits(subject, SkySaturation.some, Severity.Warning, ExposureTimeLimits.fromMax(TimeSpan.unsafeFromMicroseconds(limit / 2)))
    )).orEmpty

  /**
   * GMOS North rules.  Pass `imaging` for an imaging exposure, which is also
   * checked for sky background saturation when it is science class and the
   * filter has a saturation time.
   */
  def gmosNorth(imaging: Option[GmosImaging[GmosNorthFilter]], sky: SkyBackground): List[ExposureRule] =
    gmosNorthBase ++ gmosSkyRules(GmosNorthName, BackgroundSaturation.North, _.shortName, imaging, sky)

  /** GMOS South rules, as for `gmosNorth`. */
  def gmosSouth(imaging: Option[GmosImaging[GmosSouthFilter]], sky: SkyBackground): List[ExposureRule] =
    gmosSouthBase ++ gmosSkyRules(GmosSouthName, BackgroundSaturation.South, _.shortName, imaging, sky)

  // GNIRS

  private val gnirsRules: Map[GnirsReadMode, List[ExposureRule]] =
    Enumerated[GnirsReadMode].all.fproduct: readMode =>
      val subject = nes(s"GNIRS ${readMode.shortName.toLowerCase} read mode")
      ExposureRule.Limits(subject, none, Severity.Error, ExposureTimeLimits.fromMin(readMode.minimumExposureTime)) ::
        readMode.maximumWarningExposureTime.toList.map: t =>
          ExposureRule.Limits(subject, ReadNoise.some, Severity.Warning, ExposureTimeLimits.fromMax(t))
    .toMap

  def gnirs(readMode: GnirsReadMode): List[ExposureRule] =
    gnirsRules(readMode)

  // IGRINS-2

  // The spectrograph, used for science and calibration exposures.
  private val igrins2Spectrograph: List[ExposureRule] =
    List(
      ExposureRule.Limits(nes("IGRINS-2"), none, Severity.Error, ExposureTimeLimits.unsafeFromMinMax(Igrins2MinExposureTime, Igrins2MaxExposureTime))
    )

  // The slit viewing camera, used for acquisition exposures.
  private val igrins2Svc: List[ExposureRule] =
    List(
      ExposureRule.Limits(nes("the IGRINS-2 slit viewing camera"), none, Severity.Error, ExposureTimeLimits.unsafeFromMinMax(Igrins2SvcMinExposureTime, Igrins2MaxExposureTime))
    )

  /**
   * IGRINS-2 rules.  Acquisitions are taken with the slit viewing camera,
   * everything else with the spectrograph.
   */
  def igrins2(observeClass: ObserveClass): List[ExposureRule] =
    if observeClass === ObserveClass.Acquisition then igrins2Svc
    else igrins2Spectrograph
