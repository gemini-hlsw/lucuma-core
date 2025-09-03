// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import lucuma.catalog.BandsList
import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import eu.timepit.refined.types.string.NonEmptyString

case class BlindOffsetCandidate(
  target:          Target.Sidereal,
  angularDistance: Angle,
  baseCoordinates: Coordinates
) {
  val score: Double            = BlindOffsetScoringAlgorithm.calculateScore(this)
  def sourceId: NonEmptyString = target.name
  def coordinates: Coordinates = target.tracking.baseCoordinates
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
    baseCoordinates: Coordinates
  ): List[BlindOffsetCandidate] =
    targets
      .map { target =>
        val distance = baseCoordinates.angularDistance(target.tracking.baseCoordinates)
        BlindOffsetCandidate(target, distance, baseCoordinates)
      }
      .sortBy(_.score)
}
