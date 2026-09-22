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

import java.util.Random
import scala.collection.immutable.SortedMap
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

// Node benchmark of the AGS hot path: GMOS imaging, 36 position angles, N science offsets,
// 91 candidates. `dump` mode prints one line per analysis so two builds can be diffed.
object AgsBench:

  case class Config(offsets: List[Int], reps: Int, candidates: Int)

  val constraints: ConstraintSet = ConstraintSet(
    ImageQuality.Preset.PointEight,
    CloudExtinction.Preset.PointFive,
    SkyBackground.Dark,
    WaterVapor.Wet,
    ElevationRange.ByAirMass.Default
  )

  val wavelength: Wavelength = Wavelength.fromIntNanometers(500).get
  val params: AgsParams      = AgsParams.GmosImaging(PortDisposition.Side)
  val base: Coordinates      = Coordinates.Zero

  val posAngles: NonEmptyList[Angle] =
    NonEmptyList.fromListUnsafe(0.until(360).by(10).toList.map(d => Angle.fromDoubleDegrees(d)))

  def spiralOffsets(n: Int): List[Offset] =
    1.to(n)
      .toList
      .map: i =>
        val r = i.toDouble / n * 30.0
        val a = i * 2.399963
        Offset.signedDecimalArcseconds.reverseGet((r * math.cos(a), r * math.sin(a)))

  def scienceOffsets(n: Int): Option[ScienceOffsets] =
    NonEmptySet
      .fromSet(scala.collection.immutable.SortedSet.from(spiralOffsets(n).map(_.guided)))
      .map(ScienceOffsets(_))

  def candidates(n: Int, seed: Long): List[GuideStarCandidate] =
    val rnd = new Random(seed)
    1.to(n)
      .toList
      .map: i =>
        val p      = (rnd.nextDouble() - 0.5) * 600.0
        val q      = (rnd.nextDouble() - 0.5) * 600.0
        val coords =
          base.offsetBy(Angle.Angle0, Offset.signedDecimalArcseconds.reverseGet((p, q))).get
        val g      = BrightnessValue.unsafeFrom(
          BigDecimal(8.0 + rnd.nextDouble() * 12.0).setScale(3, BigDecimal.RoundingMode.HALF_UP)
        )
        GuideStarCandidate(i.toLong, SiderealTracking.const(coords), SortedMap(Band.Gaia -> g))

  def runFull(offsets: Int, cands: List[GuideStarCandidate]): AgsAnalysisResult =
    Ags.agsAnalysis(constraints, wavelength, base, Nil, None, posAngles, None,
                    scienceOffsets(offsets), params, cands)

  def dumpLine(a: AgsAnalysis): String =
    val vig = a match
      case u: AgsAnalysis.Usable => u.vignetting.toMicroarcsecondsSquared.toString
      case _                     => "-"
    s"${Ags.resultLabel(a)}\t${a.target.id}\t${Angle.microarcseconds.get(a.posAngle)}\t$vig"

  def ms(nanos: Long): String = f"${nanos / 1.0e6}%.1f"

  def argv: List[String] = global.process.argv.asInstanceOf[js.Array[String]].toList.drop(2)

  def main(args: Array[String]): Unit =
    val offsets = argv.headOption
      .map(_.split(",").toList.flatMap(_.trim.toIntOption))
      .filter(_.nonEmpty)
      .getOrElse(List(20, 30, 50))
    val cfg     = Config(offsets, reps = 3, candidates = 91)
    val cands   = candidates(cfg.candidates, seed = 42L)
    if argv.drop(1).headOption.contains("dump") then
      runFull(cfg.offsets.min, cands).analyses.foreach(a => println(dumpLine(a)))
      return
    println(s"node ${global.process.versions.node}  offsets=${cfg.offsets.mkString(",")} reps=${cfg.reps} candidates=${cfg.candidates}")
    println("offsets\tpositions\trep\tcalcs_ms\tcontext_ms\tanalysis_ms\ttotal_ms")
    runFull(cfg.offsets.min, cands)
    cfg.offsets.foreach: n =>
      val stats = (1 to cfg.reps).toList.map: rep =>
        val s = runFull(n, cands).stats
        println(s"$n\t${s.positionCount}\t$rep\t${ms(s.calcsNanos)}\t${ms(s.contextNanos)}\t${ms(s.analysisNanos)}\t${ms(s.contextNanos + s.analysisNanos)}")
        s
      val last  = stats.last
      val avgC  = stats.map(_.calcsNanos).sum / stats.size
      val avgA  = stats.map(_.analysisNanos).sum / stats.size
      println(s"$n\tavg\tcalcs=${ms(avgC)} analysis=${ms(avgA)} total=${ms(avgC + avgA)}")
      println(s"$n\thistogram\taccepted=${last.acceptedCount} " +
        last.resultCounts.toList.sortBy(_._1).map((k, v) => s"$k=$v").mkString(" "))
