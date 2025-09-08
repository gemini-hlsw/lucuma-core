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

    assert(score1 < score2)
    assert(score1 > 0.0)
    assert(score2 > 0.0)

  test("ranks unusable candidates last"):
    val baseSiderealTracking = SiderealTracking(
      baseCoordinates = Coordinates.Zero,
      epoch = Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    )

    val targets = List(mockTarget) // mockTarget has no magnitude data

    val baseObjectTracking = ObjectTracking.SiderealObjectTracking(baseSiderealTracking)

    val candidates = BlindOffsets.analysis(
      targets,
      baseObjectTracking,
      observationTime
    )

    assertEquals(candidates.length, 1)
    assertEquals(candidates.head.score.toDouble, Double.MaxValue)
