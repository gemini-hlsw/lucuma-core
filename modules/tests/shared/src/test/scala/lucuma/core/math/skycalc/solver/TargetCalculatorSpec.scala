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
import java.time.Instant
import org.scalactic.Tolerance
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
  private val testCoordinates = (_: Instant) =>
    Coordinates(RightAscension.fromDoubleDegrees(150), Declination.fromDoubleDegrees(20).get)

  private def singleTargetCalculator = TargetCalculator(GN, testCoordinates, testInstant)

  test("Calculates for Single Target") {
    // check definition interval
    assert(!singleTargetCalculator.isDefinedAt(testInstant.minusMillis(1)))
    assert(singleTargetCalculator.isDefinedAt(testInstant))
    assert(!singleTargetCalculator.isDefinedAt(testInstant.plusMillis(1)))

    // check some values
    assert(37.0 === singleTargetCalculator.value(_.altitudeRaw) +- 1)
    assert(1.6 === singleTargetCalculator.value(_.airmass) +- 0.1)
  }

  test("Calculates for Target in Interval") {
    Coordinates(RightAscension.fromDoubleDegrees(150), Declination.fromDoubleDegrees(20).get)
    val interval                 = Interval.unsafe(testInstant, testInstant.plusSeconds(4 * 60 * 60))
    val intervalTargetCalculator = TargetCalculator(GN, testCoordinates, interval)

    // check definition interval
    assert(!intervalTargetCalculator.isDefinedAt(interval.start.minusMillis(1)))
    assert(intervalTargetCalculator.isDefinedAt(interval.start))
    assert(intervalTargetCalculator.isDefinedAt(interval.end))

    // check some values
    assert(
      37.0 === intervalTargetCalculator
        .valueAt[LinearInterpolating](_.altitude)(testInstant)
        .toAngle
        .toSignedDoubleDegrees +- 1
    )
    assert(
      1.6 === intervalTargetCalculator.valueAt[LinearInterpolating](_.airmass)(testInstant) +- 0.1
    )
    assert(89.0 === intervalTargetCalculator.max(_.altitude).toAngle.toSignedDoubleDegrees +- 1)
    assert(37.0 === intervalTargetCalculator.min(_.altitude).toAngle.toSignedDoubleDegrees +- 1)
  }
}
