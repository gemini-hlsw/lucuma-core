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
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.refined.auto.*
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
      catalogResult = CatalogTargetResult(target1, None),
      distance = baseCoords.angularDistance(coords1),
      baseCoordinates = baseCoords,
      candidateCoords = coords1,
      observationTime = observationTime
    )

    val candidate2 = BlindOffsetCandidate(
      catalogResult = CatalogTargetResult(target2, None),
      distance = baseCoords.angularDistance(coords2),
      baseCoordinates = baseCoords,
      candidateCoords = coords2,
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

    val catalogResults =
      List(CatalogTargetResult(mockTarget, None)) // mockTarget has no magnitude data

    val baseTracking = baseSiderealTracking

    val candidates = BlindOffsets.analysis(
      catalogResults,
      baseTracking,
      observationTime
    )

    assertEquals(candidates.length, 1)
    assertEquals(candidates.head.score.toDouble, Double.MaxValue)
