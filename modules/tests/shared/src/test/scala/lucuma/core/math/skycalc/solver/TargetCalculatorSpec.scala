// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.RightAscension
import java.time.ZonedDateTime
import java.time.LocalDate
import java.time.LocalTime
import org.scalactic.Tolerance
import gsp.math.skycalc.SkyCalcResults
import java.time.Duration
import gsp.math.skycalc.solver.GetterStrategy.LinearInterpolating

/**
  * Compare some random values with results from http://catserver.ing.iac.es/staralt/index.php
  * This is not meant to test the underlying SkyCalc implementations, we assume that this is all working,
  * this only tests the general mechanics of the TargetCalculator class.
  */
final class TargetCalculatorSpec extends CatsSuite with Tolerance {
  import gsp.math.skycalc.GN

  private val testInstant     =
    ZonedDateTime.of(LocalDate.of(2014, 3, 1), LocalTime.of(20, 0, 0), GN.zone).toInstant

  private val testCoordinates =
    Coordinates(RightAscension.fromDoubleDegrees(150), Declination.fromDoubleDegrees(20).get)

  val singleTargetCalculator: Samples[SkyCalcResults] =
    Samples.single(testInstant, testCoordinates).toSkyCalResultsAt(GN)

  test("Retrieves singleton value at singleton time.") {
    assert(singleTargetCalculator.valueAt[GetterStrategy.Closest](testInstant).map(_.altitudeRaw).exists(a => 37.0 === a +- 1))
    assert(singleTargetCalculator.valueAt[GetterStrategy.Closest](testInstant).map(_.airmass).exists(a => 1.6 === a +- 0.1))
  }

  test("Calculates for Target in Interval") {
    val interval                 = Interval.unsafe(testInstant, testInstant.plusSeconds(4 * 60 * 60))
    val intervalTargetCalculator = Samples.atFixedRate(interval, Duration.ofSeconds(30L))(_ => testCoordinates).toSkyCalResultsAt(GN)

    // check some values
    assert(
      intervalTargetCalculator
        .map(_.altitude)
        .valueAt[LinearInterpolating](testInstant)
        .map(_.toAngle.toSignedDoubleDegrees)
        .exists(a => 37.0 === a +- 1)
    )
    assert(
      intervalTargetCalculator.map(_.airmass).valueAt[LinearInterpolating](testInstant).exists(a => 1.6 ===  a +- 0.1)
    )

    assert(89.0 === intervalTargetCalculator.map(_.altitude).data.values.max.map(_.toAngle.toSignedDoubleDegrees).value +- 1)
    assert(37.0 === intervalTargetCalculator.map(_.altitude).data.values.min.map(_.toAngle.toSignedDoubleDegrees).value +- 1)
  }
}
