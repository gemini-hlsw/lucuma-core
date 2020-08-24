// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import java.time.Instant
import java.time.Duration
import gsp.math.Declination
import gsp.math.HourAngle
import gsp.math.skycalc.SkyCalcResults
import cats.Eval

/**
  * This is not meant to test the underlying SkyCalc implementations, we assume that this is all working,
  * this only tests the general mechanics of the TargetCalculator class.
  */
final class ConstraintSolverSpec extends CatsSuite {
  object TestCalculator extends FixedRateCalculator[SkyCalcResults] {

    override val interval: Interval = buildInterval(0, 90)

    override val rate: Duration = Duration.ofMillis(1)

    override def result(i: Instant): Eval[SkyCalcResults] =
      Eval.now(
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
          0.0
        )
      )
  }

  test("ElevationSolver") {
    val solver =
      ElevationSolver(Declination.fromDoubleDegrees(40.0).get,
                      Declination.fromDoubleDegrees(49.0).get,
                      Duration.ofMillis(1)
      )
    val s1     = solver.solve(TestCalculator)(buildInterval(0, 90))
    assert(s1 === Schedule(buildInterval(40, 50)))
    val s2     = solver.solve(TestCalculator)(buildInterval(0, 45))
    assert(s2 === Schedule(buildInterval(40, 45)))
    val s3     = solver.solve(TestCalculator)(buildInterval(45, 90))
    assert(s3 === Schedule(buildInterval(45, 50)))
  }

  test("SkyBrightnessSolver") {
    val solver = SkyBrightnessSolver(40.0, 49.0, Duration.ofMillis(1))
    val s1     = solver.solve(TestCalculator)(buildInterval(0, 90))
    assert(s1 === Schedule(buildInterval(40, 50)))
    val s2     = solver.solve(TestCalculator)(buildInterval(0, 45))
    assert(s2 === Schedule(buildInterval(40, 45)))
    val s3     = solver.solve(TestCalculator)(buildInterval(45, 90))
    assert(s3 === Schedule(buildInterval(45, 50)))
  }

  test("AirmassSolver") {
    val solver = AirmassSolver(2.0, 15.0, Duration.ofMillis(1))
    val s1     = solver.solve(TestCalculator)(buildInterval(0, 90))
    assert(s1 === Schedule(buildInterval(5, 16)))
    val s2     = solver.solve(TestCalculator)(buildInterval(0, 10))
    assert(s2 === Schedule(buildInterval(5, 10)))
    val s3     = solver.solve(TestCalculator)(buildInterval(10, 90))
    assert(s3 === Schedule(buildInterval(10, 16)))
  }

  test("HourAngleSolver") {
    val solver =
      HourAngleSolver(HourAngle.fromHMS(2, 0, 0, 0, 0),
                      HourAngle.fromHMS(15, 0, 0, 0, 0),
                      Duration.ofMillis(1)
      )
    val s1     = solver.solve(TestCalculator)(buildInterval(0, 24))
    assert(s1 === Schedule(buildInterval(5, 16)))
    val s2     = solver.solve(TestCalculator)(buildInterval(0, 10))
    assert(s2 === Schedule(buildInterval(5, 10)))
    val s3     = solver.solve(TestCalculator)(buildInterval(10, 24))
    assert(s3 === Schedule(buildInterval(10, 16)))
    val s4     = solver.solve(TestCalculator)(buildInterval(0, 48))
    assert(s4 === Schedule(buildInterval(5, 16)).union(Schedule(buildInterval(26, 40))))
    val s5     = solver.solve(TestCalculator)(buildInterval(0, 34))
    assert(s5 === Schedule(buildInterval(5, 16)).union(Schedule(buildInterval(26, 34))))
    val s6     = solver.solve(TestCalculator)(buildInterval(10, 48))
    assert(s6 === Schedule(buildInterval(10, 16)).union(Schedule(buildInterval(26, 40))))
  }
}
