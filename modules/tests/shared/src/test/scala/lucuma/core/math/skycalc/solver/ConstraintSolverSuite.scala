// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc
package solver

import lucuma.core.enum.Site.GN
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.IntervalGens
import lucuma.core.math.skycalc.SkyCalcResults
import org.typelevel.cats.time._
import spire.math.extras.interval.IntervalSeq

import java.time.Duration

/**
 * This is not meant to test the underlying SkyCalc implementations, we assume that this is all working,
 * this only tests the general mechanics of the ConstraintSolver instances.
 */
final class ConstraintSolverSuite extends munit.DisciplineSuite with IntervalGens {

  val testSamples =
    Samples.atFixedRate(buildInterval(0, 90), Duration.ofMillis(1L)) { i =>
      SkyCalcResults(
        i.toEpochMilli.toDouble, // Elevation
        0.0,
        0.0,
        i.toEpochMilli.toDouble, // Airmass
        i.toEpochMilli.toDouble % 24, // HourAngle
        0,
        0.0,
        i.toEpochMilli.toDouble, // SkyBrightness
        0.0,
        0.0,
        0.0,
        0.0,
        Coordinates.Zero,
        GN.place
      )
    }

  test("ElevationSolver") {
    val solver =
      ElevationSolver(
        Declination.fromDoubleDegrees(40.0).get,
        Declination.fromDoubleDegrees(49.0).get,
        Duration.ofMillis(1)
      )
    val s1     = solver.solve(testSamples)(buildInterval(0, 90))
    assertEquals(s1, IntervalSeq(buildInterval(40, 50)))
    val s2     = solver.solve(testSamples)(buildInterval(0, 45))
    assertEquals(s2, IntervalSeq(buildInterval(40, 45)))
    val s3     = solver.solve(testSamples)(buildInterval(45, 90))
    assertEquals(s3, IntervalSeq(buildInterval(45, 50)))
  }

  test("SkyBrightnessSolver") {
    val solver = SkyBrightnessSolver(40.0, 49.0, Duration.ofMillis(1))
    val s1     = solver.solve(testSamples)(buildInterval(0, 90))
    assertEquals(s1, IntervalSeq(buildInterval(40, 50)))
    val s2     = solver.solve(testSamples)(buildInterval(0, 45))
    assertEquals(s2, IntervalSeq(buildInterval(40, 45)))
    val s3     = solver.solve(testSamples)(buildInterval(45, 90))
    assertEquals(s3, IntervalSeq(buildInterval(45, 50)))
  }

  test("AirmassSolver") {
    val solver = AirmassSolver(2.0, 15.0, Duration.ofMillis(1))
    val s1     = solver.solve(testSamples)(buildInterval(0, 90))
    assertEquals(s1, IntervalSeq(buildInterval(5, 16)))
    val s2     = solver.solve(testSamples)(buildInterval(0, 10))
    assertEquals(s2, IntervalSeq(buildInterval(5, 10)))
    val s3     = solver.solve(testSamples)(buildInterval(10, 90))
    assertEquals(s3, IntervalSeq(buildInterval(10, 16)))
  }

  test("HourAngleSolver") {
    val solver =
      HourAngleSolver(
        HourAngle.fromHMS(2, 0, 0, 0, 0),
        HourAngle.fromHMS(15, 0, 0, 0, 0),
        Duration.ofMillis(1)
      )
    val s1     = solver.solve(testSamples)(buildInterval(0, 24))
    assertEquals(s1, IntervalSeq(buildInterval(5, 16)))
    val s2     = solver.solve(testSamples)(buildInterval(0, 10))
    assertEquals(s2, IntervalSeq(buildInterval(5, 10)))
    val s3     = solver.solve(testSamples)(buildInterval(10, 24))
    assertEquals(s3, IntervalSeq(buildInterval(10, 16)))
    val s4     = solver.solve(testSamples)(buildInterval(0, 48))
    assertEquals(s4, IntervalSeq(buildInterval(5, 16)) | (IntervalSeq(buildInterval(26, 40))))
    val s5     = solver.solve(testSamples)(buildInterval(0, 34))
    assertEquals(s5, IntervalSeq(buildInterval(5, 16)) | (IntervalSeq(buildInterval(26, 34))))
    val s6     = solver.solve(testSamples)(buildInterval(10, 48))
    assertEquals(s6, IntervalSeq(buildInterval(10, 16)) | (IntervalSeq(buildInterval(26, 40))))
  }
}
