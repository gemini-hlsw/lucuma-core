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
  val selectionBrightness: Option[BrightnessValue] =
    SourceProfile
      .integratedBrightnessIn(limits.band)
      .headOption(catalogResult.target.sourceProfile)
      .map(_.value)

  val rejection: Option[BlindOffsetRejection] = limits.rejection(selectionBrightness)

  def isUsable: Boolean = rejection.isEmpty

  /**
   * Ranking of the candidate, lower is better. A star `MagnitudeScale` magnitudes from the optimal
   * one is penalised the same as one `SeparationScale` away from the target. Undefined without a
   * magnitude in the selection band.
   */
  val score: Option[BigDecimal] =
    selectionBrightness.map: m =>
      val magnitudeTerm = (m.value.value - limits.optimal.value.value).toDouble / MagnitudeScale
      val distanceTerm  = Angle.decimalArcseconds.get(distance).toDouble / SeparationScale
      BigDecimal(math.sqrt(magnitudeTerm * magnitudeTerm + distanceTerm * distanceTerm))
        .setScale(6, BigDecimal.RoundingMode.HALF_UP)

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

  /** Candidates for `instrument`, or none when it does not support blind offsets. */
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

        val interpreter = ADQLInterpreter.blindOffsetCandidates

        for
          results <- gaiaClient
                       .query(adqlQuery)(using interpreter)
                       .map(_.collect { case Right(result) => result })
          initial  =
            analysis(results, sedMatcher, limits, baseCoordinates, observationTime, Map.empty)
          // The score does not use Teff, so only the best candidates need an SED. A failed lookup
          // leaves them with the power law.
          params  <- gaiaClient
                       .queryStellarParameters(
                         initial
                           .filter(_.isUsable)
                           .take(StellarParameterLookups)
                           .flatMap(gaiaSourceId)
                       )
                       .handleError(_ => Map.empty)
        yield
          if params.isEmpty then initial
          else analysis(results, sedMatcher, limits, baseCoordinates, observationTime, params)

  def analysis(
    catalogResults:    List[CatalogTargetResult],
    sedMatcher:        SEDMatcher,
    limits:            BlindOffsetLimits,
    baseTracking:      Tracking,
    observationTime:   Instant,
    stellarParameters: Map[Long, GaiaStellarParameters]
  ): List[BlindOffsetCandidate] =
    baseTracking
      .at(observationTime)
      .foldMap: baseCoordinates =>
        analysis(catalogResults,
                 sedMatcher,
                 limits,
                 baseCoordinates,
                 observationTime,
                 stellarParameters
        )

  def analysis(
    catalogResults:    List[CatalogTargetResult],
    sedMatcher:        SEDMatcher,
    limits:            BlindOffsetLimits,
    baseCoordinates:   Coordinates,
    observationTime:   Instant,
    stellarParameters: Map[Long, GaiaStellarParameters]
  ): List[BlindOffsetCandidate] =
    catalogResults
      .flatMap: catalogResult =>
        catalogResult.target.tracking.at(observationTime).map { candidateCoords =>
          val distance = baseCoordinates.angularDistance(candidateCoords)
          val params   = gaiaSourceId(catalogResult.target).flatMap(stellarParameters.get)
          BlindOffsetCandidate(
            withEstimatedBrightnessesAndSed(sedMatcher, params, catalogResult),
            distance,
            baseCoordinates,
            candidateCoords,
            observationTime,
            limits,
            params
          )
        }
      .sorted

  // Gaia only has G, BP and RP and no SED. The ITC needs a brightness in a band it knows and an
  // SED, so both are estimated: magnitudes from the Gaia colour transformations, the SED from
  // the Gaia Teff and log g. Without those, or without a library match, a flat power law.
  private def withEstimatedBrightnessesAndSed(
    sedMatcher: SEDMatcher,
    params:     Option[GaiaStellarParameters],
    candidate:  CatalogTargetResult
  ): CatalogTargetResult =
    val sed: UnnormalizedSED =
      params
        .flatMap(p => sedMatcher.matchStellarParameters(p.teff, p.logG))
        .fold(UnnormalizedSED.PowerLaw(BigDecimal(0)))(UnnormalizedSED.StellarLibrary(_))

    CatalogTargetResult.target
      .andThen(Target.Sidereal.integratedBandNormalizedSpectralDefinition)
      .modify: bn =>
        val withSed = BandNormalized.sed.modify(_.orElse(sed.some))(bn)
        BandNormalized
          .brightnesses[Integrated]
          .modify: map =>
            val estimates =
              GaiaPhotometry
                .estimatedBrightnesses(map.map((band, m) => band -> m.value))
                .map((band, bv) => band -> band.defaultIntegrated.units.withValueTagged(bv))
            // Measured brightnesses are never replaced by estimates
            estimates ++ map
          .apply(withSed)
      .apply(candidate)
