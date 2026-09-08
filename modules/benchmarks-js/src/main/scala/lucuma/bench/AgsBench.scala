// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all.*
import lucuma.ags.*
import lucuma.ags.syntax.*
import lucuma.core.enums.Band
import lucuma.core.model.CloudExtinction
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.SiderealTracking

import java.util.Random
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

/**
 * Browser/Node benchmark of the AGS hot path, reproducing the workload in
 * docs/ags-parallelization.md: GMOS imaging, 36 position angles, N science offsets, 91 candidates.
 *
 * Deliberately free of cats-effect so it links under the Scala.js WebAssembly backend.
 */
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

  // Deterministic dither spiral, 30" radius so the patrol-field intersection stays non-empty
  def spiralOffsets(n: Int): List[Offset] =
    val maxRadiusArcsec = 30.0
    1.to(n).toList.map: i =>
      val t = i.toDouble / n
      val r = t * maxRadiusArcsec
      val a = i * 2.399963 // golden angle
      Offset.signedDecimalArcseconds.reverseGet((r * math.cos(a), r * math.sin(a)))

  def scienceOffsets(n: Int): Option[ScienceOffsets] =
    NonEmptySet.fromSet(scala.collection.immutable.SortedSet.from(spiralOffsets(n).map(_.guided)))
      .map(ScienceOffsets(_))

  // Candidates spread over a 10' box, Gaia G uniform in [8, 20)
  def candidates(n: Int, seed: Long): List[GuideStarCandidate] =
    val rnd = new Random(seed)
    1.to(n).toList.map: i =>
      val p      = (rnd.nextDouble() - 0.5) * 600.0
      val q      = (rnd.nextDouble() - 0.5) * 600.0
      val coords = base.offsetBy(Angle.Angle0, Offset.signedDecimalArcseconds.reverseGet((p, q))).get
      val g      = BrightnessValue.unsafeFrom(BigDecimal(8.0 + rnd.nextDouble() * 12.0).setScale(3, BigDecimal.RoundingMode.HALF_UP))
      GuideStarCandidate.unsafeApply(i.toLong, SiderealTracking.const(coords), (Band.Gaia, g).some)

  def runOnce(offsets: Int, cands: List[GuideStarCandidate]): AgsStats =
    Ags
      .agsAnalysis(
        constraints,
        wavelength,
        base,
        Nil,
        None,
        posAngles,
        None,
        scienceOffsets(offsets),
        params,
        cands
      )
      .stats

  def ms(nanos: Long): String = f"${nanos / 1.0e6}%.1f"

  def engine: String =
    val hasWasm = js.typeOf(global.WebAssembly) != "undefined"
    val runtime =
      if js.typeOf(global.process) != "undefined" && js.typeOf(global.process.versions) != "undefined"
      then s"node ${global.process.versions.node}"
      else if js.typeOf(global.navigator) != "undefined" then global.navigator.userAgent.toString
      else "unknown"
    s"$runtime (WebAssembly available: $hasWasm)"

  def parseConfig(): Config =
    def fromNode: Option[String] =
      if js.typeOf(global.process) != "undefined" then
        val argv = global.process.argv.asInstanceOf[js.Array[String]]
        argv.drop(2).headOption
      else None
    def fromBrowser: Option[String] =
      if js.typeOf(global.location) != "undefined" then
        val q = global.location.search.toString.stripPrefix("?")
        q.split("&").collectFirst { case s"offsets=$v" => v }
      else None
    val offsets = fromNode.orElse(fromBrowser)
      .map(_.split(",").toList.flatMap(_.trim.toIntOption))
      .filter(_.nonEmpty)
      .getOrElse(List(20, 30, 50))
    Config(offsets, reps = 3, candidates = 91)

  def report(line: String): Unit =
    println(line)
    if js.typeOf(global.document) != "undefined" then
      val pre = global.document.getElementById("out")
      if pre != null then pre.textContent = pre.textContent.toString + line + "\n"

  def kernelOnly: Boolean =
    if js.typeOf(global.process) != "undefined" then
      global.process.argv.asInstanceOf[js.Array[String]].drop(3).headOption.contains("kernel")
    else if js.typeOf(global.location) != "undefined" then
      global.location.search.toString.contains("kernel")
    else false

  // Load the Rust geo kernel (wasm-bindgen web target) relative to the linked main.js
  def loadKernel(): js.Promise[AgsGeoModule] =
    val mod = js.`import`[AgsGeoModule]("../../../agsgeo/pkg/agsgeo.js")
    mod.`then`[AgsGeoModule]: m =>
      val bytes: js.Promise[js.Any] =
        if js.typeOf(global.process) != "undefined" then
          js.`import`[js.Dynamic]("node:fs").`then`[js.Any]: fs =>
            val url  = js.Dynamic.newInstance(global.URL)("../../../agsgeo/pkg/agsgeo_bg.wasm", js.`import`.meta.url)
            fs.readFileSync(js.Dynamic.global.decodeURIComponent(url.pathname)).asInstanceOf[js.Any]
        else js.Promise.resolve[js.Any](js.undefined)
      bytes.`then`[AgsGeoModule](b => m.init(b).`then`[AgsGeoModule](_ => m))

  def kernelStage(cfg: Config, cands: List[GuideStarCandidate]): Unit =
    val candOffsets = cands.map(c => base.diff(c.tracking.baseCoordinates).offset)
    loadKernel().`then`[Unit]: m =>
      given AgsGeoModule = m
      val geo = new GeoWasmInterpreter
      report("offsets	kernel	chains	jts_ms	wasm_ms	speedup")
      KernelBench.run(cfg.offsets.min, 1, geo, candOffsets) // warm up
      cfg.offsets.foreach: n =>
        KernelBench.run(n, cfg.reps, geo, candOffsets).foreach(report)
    .`catch`[Unit](e => report(s"kernel stage failed: $e")): Unit

  def main(args: Array[String]): Unit =
    val cfg   = parseConfig()
    val cands = candidates(cfg.candidates, seed = 42L)
    report(s"engine: $engine")
    report(s"config: offsets=${cfg.offsets.mkString(",")} reps=${cfg.reps} candidates=${cfg.candidates}")
    if kernelOnly then { kernelStage(cfg, cands); return }
    report("offsets\tpositions\trep\tcalcs_ms\tcontext_ms\tanalysis_ms\ttotal_ms")

    // warm up the JIT on the smallest case
    runOnce(cfg.offsets.min, cands)

    cfg.offsets.foreach: n =>
      val stats = (1 to cfg.reps).toList.map: rep =>
        val s = runOnce(n, cands)
        report(
          s"$n\t${s.positionCount}\t$rep\t${ms(s.calcsNanos)}\t${ms(s.contextNanos)}\t${ms(s.analysisNanos)}\t${ms(s.contextNanos + s.analysisNanos)}"
        )
        s
      val last  = stats.last
      val avgC  = stats.map(_.calcsNanos).sum / stats.size
      val avgA  = stats.map(_.analysisNanos).sum / stats.size
      report(s"$n\tavg\tcalcs=${ms(avgC)} analysis=${ms(avgA)} total=${ms(avgC + avgA)}")
      report(
        s"$n\thistogram\taccepted=${last.acceptedCount} " +
          last.resultCounts.toList.sortBy(_._1).map((k, v) => s"$k=$v").mkString(" ")
      )
    kernelStage(cfg, cands)
