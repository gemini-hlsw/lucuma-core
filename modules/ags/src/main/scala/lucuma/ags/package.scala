// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order.given
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all.*
import coulomb.*
import lucuma.catalog.BandsList
import lucuma.catalog.BrightnessConstraints
import lucuma.catalog.FaintnessConstraint
import lucuma.catalog.SaturationConstraint
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.SkyBackground
import lucuma.core.geom.offsets.GeometryType
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.AirMass
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ImageQuality
import lucuma.core.syntax.all.*
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import org.typelevel.otel4s.Attribute

// Gaia DR3 positions are at epoch 2016.0. To include high-proper-motion
// stars that may have moved into the patrol field since 2016, we pad the
// query radius by 156 arcsecs which corresponds to Barnard's star
// moving over 15y.
val DefaultAreaBuffer: Angle = 156.arcseconds

val baseFwhm = Wavelength.fromIntNanometers(500).get

// FWHM as seen on the optical wavefront sensor (WFS)
// Operate on Double, we don't need exact precision
def wfsFwhm(sciFwhm: ImageQuality, wavelength: Wavelength): Double = {
  val coeff: Double =
    baseFwhm.toPicometers.value.value.toDouble / wavelength.toPicometers.value.value.toDouble
  ((sciFwhm.toArcSeconds.toDouble) * math.pow(coeff, -0.2)).toDouble
}

private val wvForWidestConstraints = Wavelength.fromIntNanometers(300).get

// Calculate the widest set of constraints, useful to cache catalog results.
// We get the union of all possible constraints across every guide probe at the
// slow guide speed, darkest sky background and 300nm wavelength.
val widestConstraints: BrightnessConstraints = {
  val constraints = Enumerated[GuideProbe].all.flatMap: probe =>
    Enumerated[ImageQuality.Preset].all.flatMap: iq =>
      Enumerated[CloudExtinction.Preset].all.map: ce =>
        faintLimit(
          probe,
          GuideSpeed.Slow,
          wvForWidestConstraints,
          SkyBackground.Darkest,
          iq.toImageQuality,
          ce.toCloudExtinction
        )

  // The list is never empty
  val lowest = constraints.maximumOption.get
  BrightnessConstraints(BandsList.GaiaBandsList, lowest, none)
}

/**
 * Calculates the daintness limits for a given Guide Speed/Wavelength and conditions These are based
 * on the gaia G band and described here:
 */
/**
 * Calculates the faintness limits for a given Probe / Guide Speed / Wavelength and conditions.
 * Limits are updated to account for per probe differences in April 2026.
 */
def faintLimit(
  probe:      GuideProbe,
  guideSpeed: GuideSpeed,
  wavelength: Wavelength,
  sb:         SkyBackground,
  iq:         ImageQuality,
  ce:         CloudExtinction
): FaintnessConstraint = {
  val limit = probe match {
    // The Altair AOWFS has no Gaia G limits; callers route it to `altairBrightnessConstraints`
    // and this branch only keeps the match total.
    case GuideProbe.GmosOIWFS | GuideProbe.Flamingos2OIWFS | GuideProbe.AltairAOWFS =>
      sb match {
        case SkyBackground.Darkest | SkyBackground.Dark =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              16.4 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              16.9 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              17.6 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
        case SkyBackground.Gray                         =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              16.3 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              16.8 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              17.5 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
        case SkyBackground.Bright                       =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              16.2 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              16.7 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              17.4 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
      }
    case GuideProbe.PWFS2                                                           =>
      sb match {
        case SkyBackground.Darkest | SkyBackground.Dark =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              14.7 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              15.0 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              16.3 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
        case SkyBackground.Gray                         =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              14.6 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              14.9 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              16.2 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
        case SkyBackground.Bright                       =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              14.5 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              14.8 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              16.1 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
      }
    case GuideProbe.PWFS1                                                           =>
      sb match {
        case SkyBackground.Darkest | SkyBackground.Dark =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              14.6 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              14.9 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              16.2 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
        case SkyBackground.Gray                         =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              14.5 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              14.8 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              16.1 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
        case SkyBackground.Bright                       =>
          guideSpeed match {
            case GuideSpeed.Fast   =>
              14.4 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Medium =>
              14.7 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
            case GuideSpeed.Slow   =>
              16.0 - 0.8 * wfsFwhm(iq, wavelength) - ce.toVegaMagnitude.toDouble
          }
      }
  }
  FaintnessConstraint(BrightnessValue.unsafeFrom(BigDecimal(limit)))
}

def gaiaBrightnessConstraints(
  probe:      GuideProbe,
  guideSpeed: GuideSpeed,
  wavelength: Wavelength,
  sb:         SkyBackground,
  iq:         ImageQuality,
  ce:         CloudExtinction
): BrightnessConstraints = {
  val faintness  = faintLimit(probe, guideSpeed, wavelength, sb, iq, ce)
  val saturation = SaturationConstraint(
    BrightnessValue.unsafeFrom(faintness.brightness.value.value - 6)
  )
  BrightnessConstraints(BandsList.GaiaBandsList, faintness, saturation.some)
}

def gaiaBrightnessConstraints(
  constraints: ConstraintSet,
  probe:       GuideProbe,
  guideSpeed:  GuideSpeed,
  wavelength:  Wavelength
): BrightnessConstraints =
  gaiaBrightnessConstraints(
    probe,
    guideSpeed,
    wavelength,
    constraints.skyBackground,
    constraints.imageQuality.toImageQuality,
    constraints.cloudExtinction.toCloudExtinction
  )

val UnconstrainedAngles =
  NonEmptyList
    .fromList(
      (0 until 360 by 10).map(a => Angle.fromDoubleDegrees(a.toDouble)).toList
    )
    .get

object GuidedOffset extends NewType[Offset]
type GuidedOffset = GuidedOffset.Type

object AcquisitionOffsets extends NewType[NonEmptySet[GuidedOffset]]:
  extension (a: AcquisitionOffsets)
    def withType: NonEmptySet[(GeometryType, GuidedOffset)] =
      NonEmptySet.fromSetUnsafe:
        a.value.toSortedSet.map(o => (GeometryType.AcqGuidedOffset, o))

type AcquisitionOffsets = AcquisitionOffsets.Type

object ScienceOffsets extends NewType[NonEmptySet[GuidedOffset]]:
  extension (a: ScienceOffsets)
    def withType: NonEmptySet[(GeometryType, GuidedOffset)] =
      NonEmptySet.fromSetUnsafe:
        a.value.toSortedSet.map(o => (GeometryType.SciGuidedOffset, o))

type ScienceOffsets = ScienceOffsets.Type

case class AgsAnalysisResult(analyses: List[AgsAnalysis], stats: AgsStats):
  def sortUsablePositions: List[AgsAnalysis.Usable] =
    analyses.sortUsablePositions

object AgsAnalysisResult:
  def from(
    candidateCount: Int,
    acceptedCount:  Int,
    posAngleCount:  Int,
    acqOffsetCount: Int,
    sciOffsetCount: Int,
    positionCount:  Int,
    analyses:       List[AgsAnalysis],
    contextNanos:   Long,
    calcsNanos:     Long,
    analysisNanos:  Long
  ): AgsAnalysisResult =
    val usableCandidates = analyses.collect { case u: AgsAnalysis.Usable => u.target.id }.toSet.size
    AgsAnalysisResult(
      analyses,
      AgsStats(
        candidateCount,
        acceptedCount,
        usableCandidates,
        posAngleCount,
        acqOffsetCount,
        sciOffsetCount,
        positionCount,
        analyses.size,
        analyses.groupMapReduce(Ags.resultLabel)(_ => 1)(_ + _),
        contextNanos,
        calcsNanos,
        analysisNanos
      )
    )

/**
 * Statistics for an ags run.
 *
 * @param candidateCount
 *   candidates supplied to the analysis
 * @param acceptedCount
 *   candidates that passed the brightness pre-filter
 * @param usableCandidateCount
 *   distinct candidates with at least one `Usable` outcome across all positions
 * @param posAngleCount
 *   distinct position angles evaluated
 * @param acqOffsetCount
 *   acquisition offsets contributing to position generation
 * @param sciOffsetCount
 *   science offsets contributing to position generation
 * @param positionCount
 *   distinct (pos angle, offset) positions evaluated per candidate
 * @param analysisCount
 *   `runAnalysis` invocations actually performed = accepted * positions
 * @param resultCounts
 *   histogram of outcomes keyed by [[Ags.resultLabel]]
 * @param contextNanos
 *   monotonic ns spent building the per-position geometry context
 * @param calcsNanos
 *   monotonic ns spent in posCalculations
 * @param analysisNanos
 *   monotonic ns spent in the runAnalysis loop (JTS work)
 */
case class AgsStats(
  candidateCount:       Int,
  acceptedCount:        Int,
  usableCandidateCount: Int,
  posAngleCount:        Int,
  acqOffsetCount:       Int,
  sciOffsetCount:       Int,
  positionCount:        Int,
  analysisCount:        Int,
  resultCounts:         Map[String, Int],
  contextNanos:         Long,
  calcsNanos:           Long,
  analysisNanos:        Long
):
  def format: String =
    def ms(nanos: Long): String           = f"${nanos / 1.0e6}%.2f ms"
    def pct(num: Long, den: Long): String =
      if (den <= 0) "n/a" else f"${100.0 * num / den}%.1f%%"

    val totalNanos = contextNanos + analysisNanos
    val throughput =
      if (analysisNanos <= 0) ""
      else f"  (${analysisCount / (analysisNanos / 1.0e6)}%.0f analyses/ms)"

    val outcomes =
      if (resultCounts.isEmpty) "    (none)"
      else
        resultCounts.toList
          .sortBy: (label, cnt) =>
            (-cnt, label)
          .map: (label, cnt) =>
            f"    $label%-18s $cnt%,7d  (${pct(cnt.toLong, analysisCount.toLong)})"
          .mkString("\n")

    val rows = List(
      "candidates"    -> f"${candidateCount}%,d supplied, ${acceptedCount}%,d accepted (${pct(acceptedCount.toLong, candidateCount.toLong)}%s), ${usableCandidateCount}%,d usable (${pct(usableCandidateCount.toLong, acceptedCount.toLong)}%s of accepted)",
      "geometry"      -> s"$posAngleCount pos angles, $acqOffsetCount acq + $sciOffsetCount sci offsets, $positionCount positions",
      "analyses"      -> f"${analysisCount}%,d runAnalysis calls$throughput",
      "ctx timing"    -> s"${ms(contextNanos)} (${pct(contextNanos, totalNanos)} of total)",
      "calcs timing"  -> s"${ms(calcsNanos)} (${pct(calcsNanos, totalNanos)} of total, overlay build)",
      "analysis time" -> s"${ms(analysisNanos)} (${pct(analysisNanos, totalNanos)} of total, predicate loop)",
      "total time"    -> ms(totalNanos)
    ).map((label, value) => f"  ${label + ":"}%-15s $value").mkString("\n")

    s"""|AGS stats:
        |$rows
        |  outcomes:
        |$outcomes""".stripMargin

object AgsStats:
  given Show[AgsStats] = Show.show(_.format)

  /**
   * Trace span attributes mirroring the fields of an [[AgsStats]], for callers that run the pure
   * `Ags.agsAnalysis` and want to record its results on their own span. As a companion extension it
   * is in scope wherever `AgsStats` is, so usage is just `result.stats.toSpanAttributes`.
   * {{{
   * T.span("ags.analysis", Attribute("ags.mode", params.mode), Attribute("ags.probe", params.probe.tag))
   *   .use { span =>
   *     val result = Ags.agsAnalysis(...)
   *     span.addAttributes(result.stats.toSpanAttributes*).as(result)
   *   }
   * }}}
   */
  extension (stats: AgsStats)
    def toSpanAttributes: List[Attribute[Long]] =
      List(
        Attribute("ags.candidates", stats.candidateCount.toLong),
        Attribute("ags.accepted", stats.acceptedCount.toLong),
        Attribute("ags.usable_candidates", stats.usableCandidateCount.toLong),
        Attribute("ags.pos_angles", stats.posAngleCount.toLong),
        Attribute("ags.acq_offsets", stats.acqOffsetCount.toLong),
        Attribute("ags.sci_offsets", stats.sciOffsetCount.toLong),
        Attribute("ags.positions", stats.positionCount.toLong),
        Attribute("ags.analyses", stats.analysisCount.toLong),
        Attribute("ags.context_nanos", stats.contextNanos),
        Attribute("ags.calcs_nanos", stats.calcsNanos),
        Attribute("ags.analysis_nanos", stats.analysisNanos)
      ) ++ stats.resultCounts.toList.map { case (label, cnt) =>
        Attribute(s"ags.result.$label", cnt.toLong)
      }

/** Bands a probe's brightness limits are expressed in. */
def probeBands(probe: GuideProbe): BandsList =
  probe match
    case GuideProbe.AltairAOWFS => BandsList.RBandsList
    case _                      => BandsList.GaiaBandsList

// OCS image quality buckets ("Guide Limits - OT Config.csv" rows), from the percentile the
// requested image quality corresponds to at the science wavelength.
private enum IqBucket:
  case Twenty, Seventy, EightyFive, Any

private def iqBucket(iq: ImageQuality, wavelength: Wavelength): IqBucket =
  val percentile: BigDecimal = iq.percentile(wavelength, AirMass.unsafeFrom(1)).toPercent
  if percentile <= 20 then IqBucket.Twenty
  else if percentile <= 70 then IqBucket.Seventy
  else if percentile <= 85 then IqBucket.EightyFive
  else IqBucket.Any

/**
 * Altair AOWFS faint R limits per guide speed (fast, medium, slow), transcribed from the OCS "Guide
 * Limits - OT Config.csv" Altair NGS and Altair LGS tables.
 */
private def altairFaintLimits(
  mode: AltairMode,
  iq:   IqBucket,
  sb:   SkyBackground
): (Double, Double, Double) =
  mode match
    case AltairMode.Ngs                    =>
      (iq, sb) match
        case (IqBucket.Twenty, SkyBackground.Darkest | SkyBackground.Dark)     => (11.85, 13.55, 15.05)
        case (IqBucket.Twenty, SkyBackground.Gray)                             => (11.75, 13.45, 14.95)
        case (IqBucket.Twenty, SkyBackground.Bright)                           => (11.65, 13.35, 14.85)
        case (IqBucket.Seventy, SkyBackground.Darkest | SkyBackground.Dark)    => (11.65, 13.35, 14.85)
        case (IqBucket.Seventy, SkyBackground.Gray)                            => (11.55, 13.25, 14.75)
        case (IqBucket.Seventy, SkyBackground.Bright)                          => (11.45, 13.15, 14.65)
        case (IqBucket.EightyFive, SkyBackground.Darkest | SkyBackground.Dark) =>
          (11.45, 13.15, 14.65)
        case (IqBucket.EightyFive, SkyBackground.Gray)                         => (11.35, 13.05, 14.55)
        case (IqBucket.EightyFive, SkyBackground.Bright)                       => (11.25, 12.95, 14.45)
        case (IqBucket.Any, SkyBackground.Darkest | SkyBackground.Dark)        => (11.00, 12.70, 14.20)
        case (IqBucket.Any, SkyBackground.Gray)                                => (10.90, 12.60, 14.10)
        case (IqBucket.Any, SkyBackground.Bright)                              => (10.80, 12.50, 14.00)
    case AltairMode.Lgs | AltairMode.LgsP1 =>
      (iq, sb) match
        case (IqBucket.Twenty, SkyBackground.Darkest)     => (15.90, 17.40, 18.40)
        case (IqBucket.Twenty, SkyBackground.Dark)        => (15.80, 17.30, 18.30)
        case (IqBucket.Twenty, SkyBackground.Gray)        => (15.70, 17.20, 18.20)
        case (IqBucket.Twenty, SkyBackground.Bright)      => (15.60, 17.10, 18.10)
        case (IqBucket.Seventy, SkyBackground.Darkest)    => (15.60, 17.10, 18.10)
        case (IqBucket.Seventy, SkyBackground.Dark)       => (15.50, 17.00, 18.00)
        case (IqBucket.Seventy, SkyBackground.Gray)       => (15.40, 16.90, 17.90)
        case (IqBucket.Seventy, SkyBackground.Bright)     => (15.30, 16.80, 17.80)
        case (IqBucket.EightyFive, SkyBackground.Darkest) => (15.30, 16.80, 17.30)
        case (IqBucket.EightyFive, SkyBackground.Dark)    => (15.20, 16.70, 17.20)
        case (IqBucket.EightyFive, SkyBackground.Gray)    => (15.10, 16.60, 17.10)
        case (IqBucket.EightyFive, SkyBackground.Bright)  => (15.00, 16.50, 17.00)
        case (IqBucket.Any, SkyBackground.Darkest)        => (14.30, 15.80, 16.30)
        case (IqBucket.Any, SkyBackground.Dark)           => (14.20, 15.70, 16.20)
        case (IqBucket.Any, SkyBackground.Gray)           => (14.10, 15.60, 16.10)
        case (IqBucket.Any, SkyBackground.Bright)         => (14.00, 15.50, 16.00)

// How far below the faint limit the AOWFS saturates, from the same OCS tables. NGS guides on
// bright stars routinely (with the ND filter); the LGS tip/tilt sensor tolerates far less.
private def altairSaturationAdjustment(mode: AltairMode): Double =
  mode match
    case AltairMode.Ngs                    => 17.0
    case AltairMode.Lgs | AltairMode.LgsP1 => 5.0

/**
 * Brightness limits in R for a natural guide star on the Altair AOWFS.
 */
def altairBrightnessConstraints(
  mode:       AltairMode,
  guideSpeed: GuideSpeed,
  wavelength: Wavelength,
  sb:         SkyBackground,
  iq:         ImageQuality,
  ce:         CloudExtinction
): BrightnessConstraints =
  val (fast, medium, slow) = altairFaintLimits(mode, iqBucket(iq, wavelength), sb)
  val limit: Double        =
    guideSpeed match
      case GuideSpeed.Fast   => fast
      case GuideSpeed.Medium => medium
      case GuideSpeed.Slow   => slow
  val faintness: Double    = limit - ce.toVegaMagnitude.toDouble
  BrightnessConstraints(
    BandsList.RBandsList,
    FaintnessConstraint(BrightnessValue.unsafeFrom(BigDecimal(faintness))),
    SaturationConstraint(
      BrightnessValue.unsafeFrom(BigDecimal(faintness - altairSaturationAdjustment(mode)))
    ).some
  )

/**
 * Brightness limits for a guide star on the given probe: R limits for the Altair AOWFS, Gaia G
 * limits for every other probe (including PWFS1 carrying the LGS+P1 tip/tilt star).
 */
def guideStarBrightnessConstraints(
  constraints: ConstraintSet,
  probe:       GuideProbe,
  altair:      Option[AltairMode],
  guideSpeed:  GuideSpeed,
  wavelength:  Wavelength
): BrightnessConstraints =
  (probe, altair) match
    case (GuideProbe.AltairAOWFS, Some(mode)) =>
      altairBrightnessConstraints(
        mode,
        guideSpeed,
        wavelength,
        constraints.skyBackground,
        constraints.imageQuality.toImageQuality,
        constraints.cloudExtinction.toCloudExtinction
      )
    case _                                    =>
      gaiaBrightnessConstraints(constraints, probe, guideSpeed, wavelength)
