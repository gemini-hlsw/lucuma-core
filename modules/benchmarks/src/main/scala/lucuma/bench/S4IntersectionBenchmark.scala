// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import lucuma.core.math.Offset
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * S4: the AGS slow path — an `(n-1)`-deep `∩` overlay chain built once per position angle
 */
@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
@State(Scope.Benchmark)
class S4IntersectionBenchmark:

  @Param(Array("10", "50", "100"))
  var offsets: Int = scala.compiletime.uninitialized

  private var offs: List[Offset] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setUp(): Unit =
    offs = Fixtures.spiralOffsets(offsets, Fixtures.gmosImagingFovSize, seed = 1)

  @Benchmark
  def buildAndEval(): Double =
    Fixtures.geom(Fixtures.gmosImagingPatrolIntersectionExpr(offs)).getArea
