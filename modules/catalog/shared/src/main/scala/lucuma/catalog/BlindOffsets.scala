// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.Eq
import cats.derived.*
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.simbad.SEDMatcher
import lucuma.catalog.votable.*
import lucuma.core.enums.Instrument
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.all.*
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time.*

import java.time.Instant

/**
 * A Gaia star near the science target that could be used to acquire it. The target in
 * `catalogResult` carries the estimated magnitudes and the matched SED. `stellarParameters` is only
 * looked up for the best candidates.
 */
case class BlindOffsetCandidate(
  catalogResult:     CatalogTargetResult,
  distance:          Angle,
  baseCoordinates:   Coordinates,
  candidateCoords:   Coordinates,
  observationTime:   Instant,
  limits:            BlindOffsetLimits,
  stellarParameters: Option[GaiaStellarParameters] = None
) derives Eq:
  import BlindOffsetCandidate.*

  def sourceId: NonEmptyString = catalogResult.target.name

  /** Magnitude in the instrument's selection band, estimated from Gaia photometry. */
  lazy val selectionBrightness: Option[BrightnessValue] =
    SourceProfile
      .integratedBrightnessIn(limits.band)
      .headOption(catalogResult.target.sourceProfile)
      .map(_.value)

  lazy val rejection: Option[BlindOffsetRejection] = limits.rejection(selectionBrightness)

  def isUsable: Boolean = rejection.isEmpty

  /**
   * Ranking of the candidate, lower is better. A star `MagnitudeScale` magnitudes from the optimal
   * one is penalised the same as one `SeparationScale` away from the target. Undefined without a
   * magnitude in the selection band.
   */
  lazy val score: Option[BigDecimal] =
    selectionBrightness.map: m =>
      val magnitudeTerm = (m.value.value - limits.optimal.value.value).toDouble / MagnitudeScale
      val distanceTerm  = Angle.decimalArcseconds.get(distance).toDouble / SeparationScale
      BigDecimal(math.sqrt(magnitudeTerm * magnitudeTerm + distanceTerm * distanceTerm))

object BlindOffsetCandidate:
  val MagnitudeScale: Double  = 5.0
  val SeparationScale: Double = 30.0

  // Usable candidates first, then by score, candidates without a score last. Ties go to the
  // closer star, then the source id, so the order does not depend on the catalog row order.
  given Ordering[BlindOffsetCandidate] =
    Ordering.by(c =>
      (c.rejection.isDefined,
       c.score.isEmpty,
       c.score.getOrElse(BigDecimal(0)),
       c.distance.toMicroarcseconds,
       c.sourceId.value
      )
    )

  // Optics
  val catalogResult: Lens[BlindOffsetCandidate, CatalogTargetResult]               =
    Focus[BlindOffsetCandidate](_.catalogResult)
  val distance: Lens[BlindOffsetCandidate, Angle]                                  =
    Focus[BlindOffsetCandidate](_.distance)
  val baseCoordinates: Lens[BlindOffsetCandidate, Coordinates]                     =
    Focus[BlindOffsetCandidate](_.baseCoordinates)
  val candidateCoords: Lens[BlindOffsetCandidate, Coordinates]                     =
    Focus[BlindOffsetCandidate](_.candidateCoords)
  val observationTime: Lens[BlindOffsetCandidate, Instant]                         =
    Focus[BlindOffsetCandidate](_.observationTime)
  val limits: Lens[BlindOffsetCandidate, BlindOffsetLimits]                        =
    Focus[BlindOffsetCandidate](_.limits)
  val stellarParameters: Lens[BlindOffsetCandidate, Option[GaiaStellarParameters]] =
    Focus[BlindOffsetCandidate](_.stellarParameters)

object BlindOffsets:
  val SearchRadius: Angle = 300.arcseconds

  // ESP-HS parameters are fetched in a second query for the best candidates only. Joining the
  // astrophysical parameters table into the cone search costs 15 s instead of 3 s.
  val StellarParameterLookups: Int = 10

  private val GaiaName = """Gaia DR3 (\d+)""".r

  def gaiaSourceId(target: Target.Sidereal): Option[Long] =
    target.name.value match
      case GaiaName(id) => id.toLongOption
      case _            => None

  def gaiaSourceId(candidate: BlindOffsetCandidate): Option[Long] =
    gaiaSourceId(candidate.catalogResult.target)

  /**
   * Candidates for `instrument`, or none when it does not support blind offsets. Rejected
   * candidates are kept after the usable ones, so the first one is only a valid choice if
   * `isUsable`.
   */
  def runBlindOffsetAnalysis[F[_]: Concurrent](
    gaiaClient:      GaiaClient[F],
    sedMatcher:      SEDMatcher,
    instrument:      Instrument,
    baseTracking:    Tracking,
    observationTime: Instant
  )(using ShapeInterpreter): F[List[BlindOffsetCandidate]] =
    baseTracking
      .at(observationTime)
      .map: baseCoordinates =>
        runBlindOffsetAnalysis(gaiaClient, sedMatcher, instrument, baseCoordinates, observationTime)
      .getOrElse(List.empty.pure[F])

  def runBlindOffsetAnalysis[F[_]: Concurrent](
    gaiaClient:      GaiaClient[F],
    sedMatcher:      SEDMatcher,
    instrument:      Instrument,
    baseCoordinates: Coordinates,
    observationTime: Instant
  )(using ShapeInterpreter): F[List[BlindOffsetCandidate]] =
    BlindOffsetLimits
      .forInstrument(instrument)
      .fold(List.empty.pure[F]): limits =>
        val adqlQuery = QueryByADQL(
          base = baseCoordinates,
          shapeConstraint = ShapeExpression.centeredEllipse(SearchRadius * 2, SearchRadius * 2),
          brightnessConstraints = None,
          areaBuffer = Angle.Angle0
        )

        val interpreter = ADQLInterpreter.blindOffsetCandidates(limits.faintestUsableG)

        for
          results <- gaiaClient
                       .query(adqlQuery)(using interpreter)
                       .map(_.collect { case Right(result) => result })
          initial  = analysis(results, limits, baseCoordinates, observationTime)
          // The score does not use Teff, so only the best candidates need an SED. A failed lookup
          // leaves them with the power law.
          ids      = initial.filter(_.isUsable).take(StellarParameterLookups).flatMap(gaiaSourceId)
          params  <-
            if ids.isEmpty then Map.empty.pure[F]
            else gaiaClient.queryStellarParameters(ids).handleError(_ => Map.empty)
        yield initial.map(withStellarParameters(sedMatcher, params))

  /**
   * Candidates sorted best first, with estimated magnitudes and a power-law SED. Rejected
   * candidates come last; check `isUsable` before taking the first one.
   */
  def analysis(
    catalogResults:  List[CatalogTargetResult],
    limits:          BlindOffsetLimits,
    baseTracking:    Tracking,
    observationTime: Instant
  ): List[BlindOffsetCandidate] =
    baseTracking
      .at(observationTime)
      .foldMap: baseCoordinates =>
        analysis(catalogResults, limits, baseCoordinates, observationTime)

  def analysis(
    catalogResults:  List[CatalogTargetResult],
    limits:          BlindOffsetLimits,
    baseCoordinates: Coordinates,
    observationTime: Instant
  ): List[BlindOffsetCandidate] =
    catalogResults
      .flatMap: catalogResult =>
        catalogResult.target.tracking.at(observationTime).map { candidateCoords =>
          BlindOffsetCandidate(
            withEstimatedBrightnesses(catalogResult),
            baseCoordinates.angularDistance(candidateCoords),
            baseCoordinates,
            candidateCoords,
            observationTime,
            limits
          )
        }
      .sorted

  private val candidateSed =
    BlindOffsetCandidate.catalogResult
      .andThen(CatalogTargetResult.target)
      .andThen(Target.Sidereal.integratedBandNormalizedSpectralDefinition)
      .andThen(BandNormalized.sed[Integrated])

  // Stores the looked-up parameters and, when the library has a close spectrum, uses it as SED
  def withStellarParameters(
    sedMatcher: SEDMatcher,
    params:     Map[Long, GaiaStellarParameters]
  )(candidate: BlindOffsetCandidate): BlindOffsetCandidate =
    gaiaSourceId(candidate)
      .flatMap(params.get)
      .fold(candidate): p =>
        val withParams = BlindOffsetCandidate.stellarParameters.replace(p.some)(candidate)
        sedMatcher
          .matchStellarParameters(p.teff, p.logG)
          .fold(withParams)(s =>
            candidateSed.replace(UnnormalizedSED.StellarLibrary(s).some)(withParams)
          )

  // Gaia only has G, BP and RP and no SED. The ITC needs a brightness in a band it knows and an
  // SED, so magnitudes are estimated from the Gaia colour transformations and the SED starts as a
  // flat power law. Estimates are rounded to 5 decimals, otherwise precision is lost somewhere
  // between the ODB and Explore and their targets no longer compare equal.
  private def withEstimatedBrightnesses(candidate: CatalogTargetResult): CatalogTargetResult =
    CatalogTargetResult.target
      .andThen(Target.Sidereal.integratedBandNormalizedSpectralDefinition)
      .modify: bn =>
        val withSed =
          BandNormalized.sed.modify(_.orElse(UnnormalizedSED.PowerLaw(BigDecimal(0)).some))(bn)
        BandNormalized
          .brightnesses[Integrated]
          .modify: map =>
            val estimates =
              GaiaPhotometry
                .estimatedBrightnesses(map.map((band, m) => band -> m.value))
                .map: (band, bv) =>
                  val rounded = BrightnessValue
                    .from(bv.value.value.setScale(5, BigDecimal.RoundingMode.HALF_UP))
                    .getOrElse(bv)
                  band -> band.defaultIntegrated.units.withValueTagged(rounded)
            // Measured brightnesses are never replaced by estimates
            estimates ++ map
          .apply(withSed)
      .apply(candidate)
