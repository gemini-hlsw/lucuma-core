// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc.solver

import cats.tests.CatsSuite
import java.time.LocalDate
import java.time.LocalTime
import java.time.ZonedDateTime
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Interval
import lucuma.core.math.RightAscension
import lucuma.core.math.skycalc._
import org.scalactic.Tolerance
import java.time.Duration
import lucuma.core.math.skycalc.solver.RoundStrategy._

/**
  * Compare some random values with results from http://catserver.ing.iac.es/staralt/index.php
  * This is not meant to test the underlying SkyCalc implementations, we assume that this is all working,
  * this only tests the general mechanics of specific Samples classes for targets.
  */
final class TargetSamplesSpec extends CatsSuite with Tolerance {
  import lucuma.core.enum.Site.GN

  private val testInstant =
    ZonedDateTime.of(LocalDate.of(2014, 3, 1), LocalTime.of(20, 0, 0), GN.timezone).toInstant

  private val testCoordinates =
    Coordinates(RightAscension.fromDoubleDegrees(150), Declination.fromDoubleDegrees(20).get)

  val singleSample: Samples[SkyCalcResults] =
    Samples.single(testInstant, testCoordinates).toSkyCalResultsAt(GN.place)

  test("Retrieves singleton value at singleton time.") {
    assert(
      singleSample
        .valueAt[Closest](testInstant)
        .map(_.altitudeRaw)
        .exists(a => 37.0 === a +- 1)
    )
    assert(
      singleSample
        .valueAt[Closest](testInstant)
        .map(_.airmass)
        .exists(a => 1.6 === a +- 0.1)
    )
  }

  test("Calculates for Target in Interval") {
    val interval        = Interval.unsafe(testInstant, testInstant.plusSeconds(4 * 60 * 60))
    val intervalSamples = Samples
      .atFixedRate(interval, Duration.ofSeconds(30L))(_ => testCoordinates)
      .toSkyCalResultsAt(GN.place)

    // check some values
    assert(
      intervalSamples
        .map(_.altitude)
        .valueAt[LinearInterpolating](testInstant)
        .map(_.toAngle.toSignedDoubleDegrees)
        .exists(a => 37.0 === a +- 1)
    )
    assert(
      intervalSamples
        .map(_.airmass)
        .valueAt[LinearInterpolating](testInstant)
        .exists(a => 1.6 === a +- 0.1)
    )

    assert(
      89.0 === intervalSamples
        .map(_.altitude)
        .toMap
        .values
        .max
        .map(_.toAngle.toSignedDoubleDegrees)
        .value +- 1
    )
    assert(
      37.0 === intervalSamples
        .map(_.altitude)
        .toMap
        .values
        .min
        .map(_.toAngle.toSignedDoubleDegrees)
        .value +- 1
    )
  }
}
