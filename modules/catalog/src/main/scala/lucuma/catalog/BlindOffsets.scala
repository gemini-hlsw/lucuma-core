// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.BandsList
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.*
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.model.CoordinatesAt
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.syntax.all.*

import java.time.Instant

case class BlindOffsetCandidate(
  target:          Target.Sidereal,
  distance:        Angle,
  baseCoordinates: CoordinatesAt,
  candidateCoords: CoordinatesAt,
  observationTime: Instant
) {
  val score: BigDecimal        = BlindOffsetCandidate.calculateScore(this)
  def sourceId: NonEmptyString = target.name
}

object BlindOffsetCandidate:
  def referenceBrightness(target: Target.Sidereal): Option[BigDecimal] =
    BandsList.GaiaBandsList.bands.foldMap: band =>
      SourceProfile
        .integratedBrightnessIn(band)
        .headOption(target.sourceProfile)
        .map(_.value.value.value)

  private val PiArcsecs = Angle.decimalArcseconds.get(Angle.Angle180)

  private def calculateScore(candidate: BlindOffsetCandidate): BigDecimal =
    referenceBrightness(candidate.target) match
      case Some(g) =>
        // score = sqrt(((G-12)/6)^2 + (distance / 180 arcsec)^2)
        val distance =
          Angle.decimalArcseconds.get(candidate.distance) / PiArcsecs

        val magnitudeTerm = (g - 12.0) / 6.0
        // The original formula had a square root but we don't care about the value, just the
        // relative order
        magnitudeTerm.pow(2) + distance.pow(2)

      case None =>
        Double.MaxValue

object BlindOffsets:
  def runBlindOffsetAnalysis[F[_]: Concurrent](
    gaiaClient:      GaiaClient[F],
    baseTracking:    ObjectTracking,
    observationTime: Instant
  )(using ShapeInterpreter): F[List[BlindOffsetCandidate]] =
    baseTracking
      .at(observationTime)
      .map: baseCoords =>
        val baseCoordinates = baseCoords.value
        val searchRadius    = 300.arcseconds

        val adqlQuery = QueryByADQL(
          base = baseCoordinates,
          shapeConstraint = ShapeExpression.centeredEllipse(searchRadius * 2, searchRadius * 2),
          brightnessConstraints = None
        )

        val interpreter = ADQLInterpreter.blindOffsetCandidates

        gaiaClient
          .query(adqlQuery)(using interpreter)
          .map(_.collect { case Right(result) => result.target })
          .map(analysis(_, baseTracking, observationTime))
      .getOrElse(List.empty.pure[F])

  def analysis(
    targets:         List[Target.Sidereal],
    baseTracking:    ObjectTracking,
    observationTime: Instant
  ): List[BlindOffsetCandidate] =
    baseTracking
      .at(observationTime)
      .foldMap: baseCoords =>
        val baseCoordinates = baseCoords.value
        targets
          .flatMap: target =>
            target.tracking.at(observationTime).map { candidateCoords =>
              val distance = baseCoordinates.angularDistance(candidateCoords)
              BlindOffsetCandidate(target,
                                   distance,
                                   baseCoords,
                                   CoordinatesAt(candidateCoords),
                                   observationTime
              )
            }
          .sortBy(_.score)
