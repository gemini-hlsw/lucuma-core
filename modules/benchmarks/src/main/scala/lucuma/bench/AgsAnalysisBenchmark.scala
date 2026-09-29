// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.data.NonEmptyList
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.offsets.OffsetPosition
import org.locationtech.jts.geom.GeometryOverlay
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * Full `Ags.agsAnalysis` on the workloads of the JS harness: `real`, a request captured in Explore,
 * or N, the same request with an N-point offset grid.
 */
@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
@State(Scope.Benchmark)
class AgsAnalysisBenchmark:

  @Param(Array("10", "50", "100", "real"))
  var workload: String = scala.compiletime.uninitialized

  // lucuma-jts defaults to OverlayNG; "old" selects the legacy snap-if-needed overlay.
  @Param(Array("ng", "old"))
  var overlay: String = scala.compiletime.uninitialized

  private var w: AgsWorkload                          = scala.compiletime.uninitialized
  private var positions: NonEmptyList[OffsetPosition] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setUp(): Unit =
    GeometryOverlay.OVERLAY_NG_DEFAULT = overlay == "ng"
    GeometryOverlay.setOverlayImpl(overlay)
    w = AgsWorkload.named(workload).getOrElse(sys.error(s"unknown workload $workload"))
    positions = w.positions

  @Benchmark
  def posCalculations(): Int =
    w.params.posCalculations(positions).length

  @Benchmark
  def agsAnalysis(): Int =
    w.run.analyses.size
