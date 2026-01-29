// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.Eq
import cats.derived.*
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.*
import lucuma.core.enums.Band
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.Integrated
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

case class BlindOffsetCandidate(
  catalogResult:   CatalogTargetResult,
  distance:        Angle,
  baseCoordinates: Coordinates,
  candidateCoords: Coordinates,
  observationTime: Instant
) derives Eq:
  val score: BigDecimal        = BlindOffsetCandidate.calculateScore(this)
  def sourceId: NonEmptyString = catalogResult.target.name

object BlindOffsetCandidate:
  // use the brightness from the Gaia band (G)
  def referenceBrightness(catalogResult: CatalogTargetResult): Option[BigDecimal] =
    SourceProfile
      .integratedBrightnessIn(Band.Gaia)
      .headOption(catalogResult.target.sourceProfile)
      .map(_.value.value.value)

  private val distanceDivisor: BigDecimal = BigDecimal(60)

  private def calculateScore(candidate: BlindOffsetCandidate): BigDecimal =
    referenceBrightness(candidate.catalogResult) match
      case Some(g) =>
        // score = sqrt(((G-15)/3)^2 + (distance / 60 arcsec)^2)
        val distanceTerm =
          Angle.decimalArcseconds.get(candidate.distance) / distanceDivisor

        val magnitudeTerm = (g - 15.0) / 3.0
        // The original formula had a square root but we don't care about the value, just the
        // relative order
        magnitudeTerm.pow(2) + distanceTerm.pow(2)

      case None =>
        Double.MaxValue

  // Optics
  val catalogResult: Lens[BlindOffsetCandidate, CatalogTargetResult] =
    Focus[BlindOffsetCandidate](_.catalogResult)
  val distance: Lens[BlindOffsetCandidate, Angle]                    =
    Focus[BlindOffsetCandidate](_.distance)
  val baseCoordinates: Lens[BlindOffsetCandidate, Coordinates]       =
    Focus[BlindOffsetCandidate](_.baseCoordinates)
  val candidateCoords: Lens[BlindOffsetCandidate, Coordinates]       =
    Focus[BlindOffsetCandidate](_.candidateCoords)
  val observationTime: Lens[BlindOffsetCandidate, Instant]           =
    Focus[BlindOffsetCandidate](_.observationTime)

object BlindOffsets:
  def runBlindOffsetAnalysis[F[_]: Concurrent](
    gaiaClient:      GaiaClient[F],
    baseTracking:    Tracking,
    observationTime: Instant
  )(using ShapeInterpreter): F[List[BlindOffsetCandidate]] =
    baseTracking
      .at(observationTime)
      .map: baseCoordinates =>
        runBlindOffsetAnalysis(gaiaClient, baseCoordinates, observationTime)
      .getOrElse(List.empty.pure[F])

  def runBlindOffsetAnalysis[F[_]: Concurrent](
    gaiaClient:      GaiaClient[F],
    baseCoordinates: Coordinates,
    observationTime: Instant
  )(using ShapeInterpreter): F[List[BlindOffsetCandidate]] =
    val searchRadius = 300.arcseconds

    val adqlQuery = QueryByADQL(
      base = baseCoordinates,
      shapeConstraint = ShapeExpression.centeredEllipse(searchRadius * 2, searchRadius * 2),
      brightnessConstraints = None,
      areaBuffer = Angle.Angle0
    )

    val interpreter = ADQLInterpreter.blindOffsetCandidates

    gaiaClient
      .query(adqlQuery)(using interpreter)
      .map(_.collect { case Right(result) => result })
      .map(analysis(_, baseCoordinates, observationTime))

  def analysis(
    catalogResults:  List[CatalogTargetResult],
    baseTracking:    Tracking,
    observationTime: Instant
  ): List[BlindOffsetCandidate] =
    baseTracking
      .at(observationTime)
      .foldMap: baseCoordinates =>
        analysis(catalogResults, baseCoordinates, observationTime)

  def analysis(
    catalogResults:  List[CatalogTargetResult],
    baseCoordinates: Coordinates,
    observationTime: Instant
  ): List[BlindOffsetCandidate] =
    catalogResults
      .flatMap: catalogResult =>
        catalogResult.target.tracking.at(observationTime).map { candidateCoords =>
          val distance = baseCoordinates.angularDistance(candidateCoords)
          BlindOffsetCandidate(
            fakeSedAndBrightness(catalogResult),
            distance,
            baseCoordinates,
            candidateCoords,
            observationTime
          )
        }
      .sortBy(_.score)

  // See shortcut 7655
  // The Gaia catalog only has Gaia, GaiaBP and GaiaRP brightnesses and an SED is not specified.
  // Unfortunately the ITC does not yet use the `G` bands, so while Andy is fixing that, we'll
  // "pretend" the Gaia band is a V band.
  // Also we'll default the SED.
  // Once the ITC is updated, we can remove the Gaia => V part. But, we may always need to
  // fake the SED? We can figure that out at that point.
  private def fakeSedAndBrightness(candidate: CatalogTargetResult): CatalogTargetResult =
    CatalogTargetResult.target
      .andThen(Target.Sidereal.integratedBandNormalizedSpectralDefinition)
      .modify(bn =>
        // Add an SED if there isn't one.
        val withSed = BandNormalized.sed
          .modify(_.orElse(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.M0V_new).some))(bn)
        BandNormalized
          .brightnesses[Integrated]
          .modify(map =>
            // pretend the 'G' band is a V band
            map.get(Band.Gaia).fold(map)(g => map.updated(Band.V, g))
          )(withSed)
      )(candidate)
