// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.syntax.all.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.CoordinatesAt
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.refined.*
import munit.CatsEffectSuite

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset
import scala.collection.immutable.SortedMap

class BlindOffsetCandidatesSuite extends CatsEffectSuite:

  private val observationTime: Instant =
    LocalDate.of(2025, 9, 4).atStartOfDay(ZoneOffset.UTC).toInstant()

  private val mockTarget = Target.Sidereal(
    name = "Test Star".refined,
    tracking = SiderealTracking(
      baseCoordinates = Coordinates.Zero,
      epoch = Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    ),
    sourceProfile = SourceProfile.Point(SpectralDefinition.BandNormalized(None, SortedMap.empty)),
    catalogInfo = None
  )

  test("calculates scores"):
    val baseCoords = (
      RightAscension.fromStringHMS.getOption("05:35:17.3"),
      Declination.fromStringSignedDMS.getOption("+22:00:52.2")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

    val coords1 = (
      RightAscension.fromStringHMS.getOption("05:35:18.0"),
      Declination.fromStringSignedDMS.getOption("+22:00:53.0")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

    val coords2 = (
      RightAscension.fromStringHMS.getOption("05:35:30.0"),
      Declination.fromStringSignedDMS.getOption("+22:01:40.0")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

    // Create targets with G magnitude data for testing
    val gMagnitude  =
      Band.Gaia.defaultUnits[Integrated].withValueTagged(BrightnessValue.unsafeFrom(15.0))
    val spectralDef = SpectralDefinition.BandNormalized(None, SortedMap(Band.Gaia -> gMagnitude))

    val target1 = Target.Sidereal(
      name = "Star1".refined,
      tracking = SiderealTracking(coords1, Epoch.J2000, None, None, None),
      sourceProfile = SourceProfile.Point(spectralDef),
      catalogInfo = None
    )

    val target2 = Target.Sidereal(
      name = "Star2".refined,
      tracking = SiderealTracking(coords2, Epoch.J2000, None, None, None),
      sourceProfile = SourceProfile.Point(spectralDef),
      catalogInfo = None
    )

    val candidate1 = BlindOffsetCandidate(
      target = target1,
      distance = baseCoords.angularDistance(coords1),
      baseCoordinates = CoordinatesAt(baseCoords),
      candidateCoords = CoordinatesAt(coords1),
      observationTime = observationTime
    )

    val candidate2 = BlindOffsetCandidate(
      target = target2,
      distance = baseCoords.angularDistance(coords2),
      baseCoordinates = CoordinatesAt(baseCoords),
      candidateCoords = CoordinatesAt(coords2),
      observationTime = observationTime
    )

    val score1 = candidate1.score
    val score2 = candidate2.score

    // Verify score calculations - candidate1 is closer so should have lower score
    assert(score1 < score2,
           s"Closer candidate score ($score1) should be less than farther candidate score ($score2)"
    )
    assert(score1 > 0.0, "Score should be positive")
    assert(score2 > 0.0, "Score should be positive")

  test("BlindOffsetScoringAlgorithm sortCandidates returns candidates ordered by score"):
    val baseCoords = (
      RightAscension.fromStringHMS.getOption("05:35:17.3"),
      Declination.fromStringSignedDMS.getOption("+22:00:52.2")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

    val coords1 = (
      RightAscension.fromStringHMS.getOption("05:35:18.0"),
      Declination.fromStringSignedDMS.getOption("+22:00:53.0")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero) // Best candidate
    val coords2 = (
      RightAscension.fromStringHMS.getOption("05:35:30.0"),
      Declination.fromStringSignedDMS.getOption("+22:01:40.0")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero) // Worst candidate
    val coords3 = (
      RightAscension.fromStringHMS.getOption("05:35:16.0"),
      Declination.fromStringSignedDMS.getOption("+22:00:50.0")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero) // Medium candidate

    // Create targets with G magnitude data for testing
    val gMagnitude  =
      Band.Gaia.defaultUnits[Integrated].withValueTagged(BrightnessValue.unsafeFrom(15.0))
    val spectralDef = SpectralDefinition.BandNormalized(None, SortedMap(Band.Gaia -> gMagnitude))

    val targetWithMagnitude1 = Target.Sidereal(
      name = "Star1".refined,
      tracking = SiderealTracking(coords1, Epoch.J2000, None, None, None),
      sourceProfile = SourceProfile.Point(spectralDef),
      catalogInfo = None
    )
    val targetWithMagnitude2 = Target.Sidereal(
      name = "Star2".refined,
      tracking = SiderealTracking(coords2, Epoch.J2000, None, None, None),
      sourceProfile = SourceProfile.Point(spectralDef),
      catalogInfo = None
    )
    val targetWithMagnitude3 = Target.Sidereal(
      name = "Star3".refined,
      tracking = SiderealTracking(coords3, Epoch.J2000, None, None, None),
      sourceProfile = SourceProfile.Point(spectralDef),
      catalogInfo = None
    )

    val candidates = List(
      BlindOffsetCandidate(targetWithMagnitude2,
                           baseCoords.angularDistance(coords2),
                           CoordinatesAt(baseCoords),
                           CoordinatesAt(coords2),
                           observationTime
      ), // Worst
      BlindOffsetCandidate(targetWithMagnitude1,
                           baseCoords.angularDistance(coords1),
                           CoordinatesAt(baseCoords),
                           CoordinatesAt(coords1),
                           observationTime
      ), // Best
      BlindOffsetCandidate(targetWithMagnitude3,
                           baseCoords.angularDistance(coords3),
                           CoordinatesAt(baseCoords),
                           CoordinatesAt(coords3),
                           observationTime
      )  // Medium
    )

    val sorted = BlindOffsetCandidate.sortCandidates(candidates)

    // Verify sorting (best score first)
    assertEquals(sorted.length, 3)

    // Verify scores are ascending
    val scores = sorted.map(_.score)
    assertEquals(scores, scores.sorted)

  test("BlindOffsetCandidate basic properties work correctly"):
    val baseCoords   = (
      RightAscension.fromStringHMS.getOption("05:35:17.3"),
      Declination.fromStringSignedDMS.getOption("+22:00:52.2")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)
    val targetCoords = (
      RightAscension.fromStringHMS.getOption("05:35:18.0"),
      Declination.fromStringSignedDMS.getOption("+22:00:53.0")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

    // Create a target with the expected coordinates
    val targetWithCoords = Target.Sidereal(
      name = "Test Star".refined,
      tracking = SiderealTracking(
        baseCoordinates = targetCoords, // Use targetCoords instead of Coordinates.Zero
        epoch = Epoch.J2000,
        properMotion = None,
        radialVelocity = None,
        parallax = None
      ),
      sourceProfile = SourceProfile.Point(SpectralDefinition.BandNormalized(None, SortedMap.empty)),
      catalogInfo = None
    )

    val candidate = BlindOffsetCandidate(
      target = targetWithCoords,
      distance = baseCoords.angularDistance(targetCoords),
      baseCoordinates = CoordinatesAt(baseCoords),
      candidateCoords = CoordinatesAt(targetCoords),
      observationTime = observationTime
    )

    assert(candidate.score > 0.0, "Score should be positive")

  test("BlindOffsetCandidate without G magnitude gets worst score"):
    val baseCoords   = Coordinates.Zero
    val targetCoords = (
      RightAscension.fromStringHMS.getOption("05:35:18.0"),
      Declination.fromStringSignedDMS.getOption("+22:00:53.0")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

    // Target without any magnitude information (empty SourceProfile)
    val targetWithoutMagnitude = Target.Sidereal(
      name = "Star Without Magnitude".refined,
      tracking = SiderealTracking(
        baseCoordinates = targetCoords,
        epoch = Epoch.J2000,
        properMotion = None,
        radialVelocity = None,
        parallax = None
      ),
      sourceProfile = SourceProfile.Point(SpectralDefinition.BandNormalized(None, SortedMap.empty)),
      catalogInfo = None
    )

    val distance  = baseCoords.angularDistance(targetCoords)
    val candidate = BlindOffsetCandidate(
      targetWithoutMagnitude,
      distance,
      CoordinatesAt(baseCoords),
      CoordinatesAt(targetCoords),
      observationTime
    )

    // Should get the worst possible score (Double.MaxValue) because no G magnitude is available
    assertEquals(candidate.score.toDouble, Double.MaxValue)

  test("sortCandidatesFromTargets ranks unusable candidates last"):
    val baseSiderealTracking = SiderealTracking(
      baseCoordinates = Coordinates.Zero,
      epoch = Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    )

    // Create a mix of targets - some with magnitudes, some without
    val targets = List(mockTarget) // mockTarget has no magnitude data

    val baseObjectTracking = ObjectTracking.SiderealObjectTracking(baseSiderealTracking)

    val candidates = BlindOffsetCandidate.sortCandidatesFromTargets(
      targets,
      baseObjectTracking,
      observationTime
    )

    // Should return candidates but ones without magnitude will have the worst scores
    assertEquals(candidates.length, 1)
    assertEquals(candidates.head.score.toDouble, Double.MaxValue)
