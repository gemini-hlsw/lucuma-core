// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.BandsList
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target

import java.time.Instant

case class BlindOffsetCandidate(
  target:          Target.Sidereal,
  angularDistance: Angle,
  baseCoordinates: Coordinates,
  candidateCoords: Coordinates,
  observationTime: Instant
) {
  val score: Double            = BlindOffsetScoringAlgorithm.calculateScore(this)
  def sourceId: NonEmptyString = target.name
  def coordinates: Coordinates = candidateCoords
}

object BlindOffsetCandidate:
  def extractGMagnitude(target: Target.Sidereal): Option[BigDecimal] =
    BandsList.GaiaBandsList.bands.foldMap: band =>
      SourceProfile
        .integratedBrightnessIn(band)
        .headOption(target.sourceProfile)
        .map(_.value.value.value)

object BlindOffsetScoringAlgorithm {
  def calculateScore(candidate: BlindOffsetCandidate): Double =
    BlindOffsetCandidate.extractGMagnitude(candidate.target) match {
      case Some(g) =>
        // score = sqrt(((G-12)/6)^2 + (distance / 180 arcsec)^2)
        val distance = Angle.decimalArcseconds.get(
          candidate.angularDistance
        ) / Angle.decimalArcseconds.get(Angle.Angle180)

        val magnitudeTerm = (g - 12.0) / 6.0
        math.sqrt((magnitudeTerm.pow(2) + distance.pow(2)).toDouble)
      case None    =>
        Double.MaxValue
    }

  def sortCandidates(candidates: List[BlindOffsetCandidate]): List[BlindOffsetCandidate] =
    candidates.sortBy(_.score)

  def sortCandidatesFromTargets(
    targets:         List[Target.Sidereal],
    baseTracking:    ObjectTracking,
    observationTime: Instant
  ): List[BlindOffsetCandidate] =
    baseTracking.at(observationTime) match {
      case Some(baseCoords) =>
        val baseCoordinates = baseCoords.value
        targets
          .flatMap { target =>
            target.tracking.at(observationTime).map { candidateCoords =>
              val distance = baseCoordinates.angularDistance(candidateCoords)
              BlindOffsetCandidate(target,
                                   distance,
                                   baseCoordinates,
                                   candidateCoords,
                                   observationTime
              )
            }
          }
          .sortBy(_.score)
      case None             => List.empty
    }
}
