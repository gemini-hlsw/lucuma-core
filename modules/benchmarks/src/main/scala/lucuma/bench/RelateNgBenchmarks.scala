// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * JMH micro-benchmarks of 4 AGS scenarios
 *
 *   - S1 `polygon.contains(point)` × N candidate stars (stateless)
 *   - S2 `probeArm.intersects(science)` vignetting
 *   - S3 `probeArm.intersection(science).getArea` patrol-field overlap area (overlay)
 *   - S5 `IndexedPointInAreaLocator.locate` the prepared point-in-polygon ceiling
 */
@State(Scope.Benchmark)
class GeomState:
  // Built once per run
  var gmosPoints: Array[Geometry]        = scala.compiletime.uninitialized
  var gmosCoords: Array[Coordinate]      = scala.compiletime.uninitialized
  var f2Points: Array[Geometry]          = scala.compiletime.uninitialized
  var gmosArms: Array[Geometry]          = scala.compiletime.uninitialized
  var locator: IndexedPointInAreaLocator = scala.compiletime.uninitialized

  private var i         = 0
  def next(n: Int): Int =
    val r = i % n
    i += 1
    r

  @Setup(Level.Trial)
  def setUp(): Unit =
    import Fixtures.*
    val N           = 4000
    val MaxStars    = 500
    val gmosOffsets = testGuideStarOffsets(gmosPatrolFieldShape, N, seed = 1L)
    val f2Offsets   = testGuideStarOffsets(f2PatrolFieldShape, N, seed = 2L)
    gmosPoints = offsetsToPoints(gmosOffsets)
    gmosCoords = offsetsToCoords(gmosOffsets)
    f2Points = offsetsToPoints(f2Offsets)
    val reachable   = gmosOffsets.filter(gmosPatrolFieldShape.contains).take(MaxStars)
    gmosArms = reachable.map(gs => geom(gmosProbeArmExpr(gs)))
    locator = new IndexedPointInAreaLocator(gmosPatrolFieldGeom)

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class RelateNgBenchmarks:
  import Fixtures.*

  @Benchmark
  def s1_gmosContains(s: GeomState): Boolean = // S1 GMOS
    gmosPatrolFieldGeom.contains(s.gmosPoints(s.next(s.gmosPoints.length)))

  @Benchmark
  def s1_f2Contains(s: GeomState): Boolean = // S1 Flamingos2
    f2PatrolFieldGeom.contains(s.f2Points(s.next(s.f2Points.length)))

  @Benchmark
  def s2_intersects(s: GeomState): Boolean = // S2
    s.gmosArms(s.next(s.gmosArms.length)).intersects(scienceTargetGeom)

  @Benchmark
  def s3_intersectionArea(s: GeomState): Double = // S3 (overlay)
    s.gmosArms(s.next(s.gmosArms.length)).intersection(scienceTargetGeom).getArea

  @Benchmark
  def s5_indexedLocator(s: GeomState): Int = // S5
    s.locator.locate(s.gmosCoords(s.next(s.gmosCoords.length)))
