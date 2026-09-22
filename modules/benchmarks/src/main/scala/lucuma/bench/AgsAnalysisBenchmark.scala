// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import lucuma.ags.*
import lucuma.ags.syntax.*
import lucuma.core.enums.Band
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.SiderealTracking
import org.openjdk.jmh.annotations.*

import java.util.Random
import java.util.concurrent.TimeUnit
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

/**
 * Full `Ags.agsAnalysis` on the workload of the JS harness: GMOS imaging, 36 position angles,
 * N science offsets on a 30" spiral, 91 candidates with Gaia G in [8, 20).
 */
@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
@State(Scope.Benchmark)
class AgsAnalysisBenchmark:

  @Param(Array("10", "50", "100"))
  var offsets: Int = scala.compiletime.uninitialized

  private val constraints = ConstraintSet(
    ImageQuality.Preset.PointEight,
    CloudExtinction.Preset.PointFive,
    SkyBackground.Dark,
    WaterVapor.Wet,
    ElevationRange.ByAirMass.Default
  )

  private val wavelength = Wavelength.fromIntNanometers(500).get
  private val params     = AgsParams.GmosImaging(PortDisposition.Side)
  private val base       = Coordinates.Zero

  private val posAngles: NonEmptyList[Angle] =
    NonEmptyList.fromListUnsafe(0.until(360).by(10).toList.map(d => Angle.fromDoubleDegrees(d)))

  private var scienceOffsets: Option[ScienceOffsets]  = scala.compiletime.uninitialized
  private var positions: NonEmptyList[lucuma.core.geom.offsets.OffsetPosition] =
    scala.compiletime.uninitialized
  private var candidates: List[GuideStarCandidate]     = scala.compiletime.uninitialized

  private def spiral(n: Int): List[Offset] =
    1.to(n).toList.map: i =>
      val r = i.toDouble / n * 30.0
      val a = i * 2.399963
      Offset.signedDecimalArcseconds.reverseGet((r * math.cos(a), r * math.sin(a)))

  private def stars(n: Int, seed: Long): List[GuideStarCandidate] =
    val rnd = new Random(seed)
    1.to(n).toList.map: i =>
      val p      = (rnd.nextDouble() - 0.5) * 600.0
      val q      = (rnd.nextDouble() - 0.5) * 600.0
      val coords =
        base.offsetBy(Angle.Angle0, Offset.signedDecimalArcseconds.reverseGet((p, q))).get
      val g      = BrightnessValue.unsafeFrom(
        BigDecimal(8.0 + rnd.nextDouble() * 12.0).setScale(3, BigDecimal.RoundingMode.HALF_UP)
      )
      GuideStarCandidate(i.toLong, SiderealTracking.const(coords), SortedMap(Band.Gaia -> g))

  @Setup(Level.Trial)
  def setUp(): Unit =
    scienceOffsets = NonEmptySet
      .fromSet(SortedSet.from(spiral(offsets).map(_.guided)))
      .map(ScienceOffsets(_))
    positions = Ags
      .generatePositions(Some(base), None, posAngles, None, scienceOffsets)
      .value
      .toNonEmptyList
    candidates = stars(91, seed = 42L)

  @Benchmark
  def posCalculations(): Int =
    params.posCalculations(positions).length

  @Benchmark
  def agsAnalysis(): Int =
    Ags
      .agsAnalysis(constraints, wavelength, base, Nil, None, posAngles, None, scienceOffsets,
                   params, candidates)
      .analyses
      .size
