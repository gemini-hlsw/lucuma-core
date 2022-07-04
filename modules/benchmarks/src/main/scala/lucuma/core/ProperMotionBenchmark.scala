// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.model._
import lucuma.core.math._
import org.openjdk.jmh.annotations._

import java.time.Instant
import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 4, time = 1)
@Measurement(iterations = 10, time = 10)
class ProperMotionBenchmark {

  val tracking = SiderealTracking(
    Coordinates.Zero,
    Epoch.J2000,
    Some(ProperMotion.milliarcsecondsPerYear.reverseGet((3000, 4000))),
    RadialVelocity.kilometerspersecond.getOption(1e6),
    Some(Parallax.fromMicroarcseconds(10000000L))
  )

  val instant = Instant.now()

  @Benchmark
  def simpleRun: Unit = {
    tracking.at(instant)
    ()
  }

}

// Baseline
//
// [info] Result "lucuma.core.ProperMotionBenchmark.simpleRun":
// [info]   107.101 ±(99.9%) 0.192 ops/ms [Average]
// [info]   (min, avg, max) = (106.867, 107.101, 107.230), stdev = 0.127
// [info]   CI (99.9%): [106.909, 107.293] (assumes normal distribution)
// [info] # Run complete. Total time: 00:01:45
//
// ProperMotion.toRadians
//
// [info] Result "lucuma.core.ProperMotionBenchmark.simpleRun":
// [info]   3251.794 ±(99.9%) 111.004 ops/ms [Average]
// [info]   (min, avg, max) = (3103.327, 3251.794, 3311.694), stdev = 73.422
// [info]   CI (99.9%): [3140.790, 3362.798] (assumes normal distribution)
// [info] # Run complete. Total time: 00:01:45
//
// RadialVelocity.toDoubleKilometersPerSecond
// [info] Result "lucuma.core.ProperMotionBenchmark.simpleRun":
// [info]   4752.024 ±(99.9%) 40.378 ops/ms [Average]
// [info]   (min, avg, max) = (4677.622, 4752.024, 4765.228), stdev = 26.708
// [info]   CI (99.9%): [4711.645, 4792.402] (assumes normal distribution)
// [info] # Run complete. Total time: 00:01:45
//
// Inline trigonometric functions
// [info] Result "lucuma.core.ProperMotionBenchmark.simpleRun":
// [info]   5013.637 ±(99.9%) 5.885 ops/ms [Average]
// [info]   (min, avg, max) = (5010.050, 5013.637, 5021.292), stdev = 3.892
// [info]   CI (99.9%): [5007.753, 5019.522] (assumes normal distribution)
// [info] # Run complete. Total time: 00:01:45
