// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.unsafe.IORuntime
import lucuma.ags.*
import lucuma.ags.syntax.*
import lucuma.core.enums.Band
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.jts.JtsShapeInterpreter
import lucuma.core.geom.wasm.WasmGeometry
import lucuma.core.geom.wasm.WasmShapeInterpreter
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

// Node/browser benchmark of the AGS hot path: GMOS imaging, 36 position angles, N science
// offsets, 91 candidates. Modes: none (JTS timing), `wasm` (paired JTS vs kernel timing),
// `dump [jts|wasm]` (one line per analysis, for diffing builds or engines).
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

  // Candidates spread over a 10' box, Gaia G uniform in [8, 20)
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

  def runFull(offsets: Int, cands: List[GuideStarCandidate])(using
    ShapeInterpreter
  ): AgsAnalysisResult =
    Ags.agsAnalysis(constraints,
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

  def dumpLine(a: AgsAnalysis): String =
    val vig = a match
      case u: AgsAnalysis.Usable => u.vignetting.toMicroarcsecondsSquared.toString
      case _                     => "-"
    s"${Ags.resultLabel(a)}\t${a.target.id}\t${Angle.microarcseconds.get(a.posAngle)}\t$vig"

  def ms(nanos: Long): String = f"${nanos / 1.0e6}%.1f"

  def isNode: Boolean = js.typeOf(global.process) != "undefined"

  def runtime: String =
    if isNode then s"node ${global.process.versions.node}"
    else global.navigator.userAgent.toString

  def argv: List[String] =
    if isNode then global.process.argv.asInstanceOf[js.Array[String]].toList.drop(2)
    else if js.typeOf(global.location) != "undefined" then
      // ?offsets=20,30&wasm maps onto the Node argument list
      val q = global.location.search.toString.stripPrefix("?").split("&").toList
      q.collectFirst { case s"offsets=$v" => v }.toList ++ Option.when(q.contains("wasm"))("wasm")
    else Nil

  def report(line: String): Unit =
    println(line)
    if js.typeOf(global.document) != "undefined" then
      val pre = global.document.getElementById("out")
      if pre != null then pre.textContent = pre.textContent.toString + line + "\n"

  // Node cannot fetch the package's own file: URL; hand the loader the bytes. Browsers resolve it.
  def wasmBytes(): js.Promise[js.UndefOr[js.Any]] =
    if isNode then
      js.`import`[js.Dynamic]("node:fs")
        .`then`[js.UndefOr[js.Any]]: fs =>
          val url = js.`import`.meta
            .asInstanceOf[js.Dynamic]
            .resolve("lucuma-geo-wasm/lucuma_geo_wasm_bg.wasm")
          fs.readFileSync(js.Dynamic.newInstance(global.URL)(url)).asInstanceOf[js.Any]
    else js.Promise.resolve[js.UndefOr[js.Any]](js.undefined)

  def loadKernel(): js.Promise[ShapeInterpreter] =
    given IORuntime = cats.effect.unsafe.implicits.global
    wasmBytes().`then`[ShapeInterpreter](b => WasmGeometry.loadFrom(b).unsafeToPromise())

  def histogram(s: AgsStats): String =
    s"accepted=${s.acceptedCount} " + s.resultCounts.toList
      .sortBy(_._1)
      .map((k, v) => s"$k=$v")
      .mkString(" ")

  // End to end agsAnalysis on JTS and on the wasm kernel, paired per rep.
  def wasmStage(cfg: Config, cands: List[GuideStarCandidate], wasm: ShapeInterpreter): Unit =
    val engines = List("jts" -> JtsShapeInterpreter, "wasm" -> wasm)
    report(
      s"kernel: lucuma-geo-wasm, default interpreter installed: ${ShapeInterpreter.default eq wasm}"
    )
    report(f"kernel memory after load: ${WasmShapeInterpreter.memoryBytes / 1048576.0}%.1f MB")
    report(
      "offsets\tengine\trep\tcalcs_ms\tcontext_ms\tanalysis_ms\ttotal_ms\tlive_handles\twasm_mb"
    )
    engines.foreach((_, si) => runFull(cfg.offsets.min, cands)(using si))
    cfg.offsets.foreach: n =>
      val stats       = (1 to cfg.reps).toList.flatMap: rep =>
        engines.map: (name, si) =>
          val before = WasmShapeInterpreter.liveHandles
          val s      = runFull(n, cands)(using si).stats
          val live   = WasmShapeInterpreter.liveHandles - before
          val mb     = f"${WasmShapeInterpreter.memoryBytes / 1048576.0}%.1f"
          report(
            s"$n\t$name\t$rep\t${ms(s.calcsNanos)}\t${ms(s.contextNanos)}\t${ms(s.analysisNanos)}\t${ms(s.contextNanos + s.analysisNanos)}\t$live\t$mb"
          )
          name -> s
      val avg         = engines.map: (name, _) =>
        val ss = stats.collect { case (`name`, s) => s }
        (name, ss.map(_.calcsNanos).sum / ss.size, ss.map(_.analysisNanos).sum / ss.size)
      avg.foreach: (name, c, a) =>
        report(s"$n\t$name\tavg\tcalcs=${ms(c)} analysis=${ms(a)} total=${ms(c + a)}")
      val (_, jc, ja) = avg.find(_._1 == "jts").get
      val (_, wc, wa) = avg.find(_._1 == "wasm").get
      report(
        f"$n\tspeedup\tcalcs=${jc.toDouble / wc}%.2fx analysis=${ja.toDouble / wa}%.2fx total=${(jc + ja).toDouble / (wc + wa)}%.2fx"
      )
      val hists       =
        engines.map((name, _) => name -> histogram(stats.findLast(_._1 == name).get._2))
      hists.foreach((name, h) => report(s"$n\thistogram\t$name\t$h"))
      if hists.map(_._2).distinct.size != 1 then report(s"$n\tHISTOGRAM MISMATCH between engines")

  def jtsStage(cfg: Config, cands: List[GuideStarCandidate]): Unit =
    report("offsets\tpositions\trep\tcalcs_ms\tcontext_ms\tanalysis_ms\ttotal_ms")
    runFull(cfg.offsets.min, cands)
    cfg.offsets.foreach: n =>
      val stats = (1 to cfg.reps).toList.map: rep =>
        val s = runFull(n, cands).stats
        report(
          s"$n\t${s.positionCount}\t$rep\t${ms(s.calcsNanos)}\t${ms(s.contextNanos)}\t${ms(s.analysisNanos)}\t${ms(s.contextNanos + s.analysisNanos)}"
        )
        s
      val last  = stats.last
      val avgC  = stats.map(_.calcsNanos).sum / stats.size
      val avgA  = stats.map(_.analysisNanos).sum / stats.size
      report(s"$n\tavg\tcalcs=${ms(avgC)} analysis=${ms(avgA)} total=${ms(avgC + avgA)}")
      report(s"$n\thistogram\t${histogram(last)}")

  def dumpStage(cfg: Config, cands: List[GuideStarCandidate])(using ShapeInterpreter): Unit =
    runFull(cfg.offsets.min, cands).analyses.foreach(a => println(dumpLine(a)))

  def main(args: Array[String]): Unit =
    val offsets = argv.headOption
      .map(_.split(",").toList.flatMap(_.trim.toIntOption))
      .filter(_.nonEmpty)
      .getOrElse(List(20, 30, 50))
    val cfg     = Config(offsets, reps = 3, candidates = 91)
    val cands   = candidates(cfg.candidates, seed = 42L)
    val mode    = argv.drop(1)
    mode match
      case "dump" :: "wasm" :: _ =>
        loadKernel()
          .`then`[Unit](wasm => dumpStage(cfg, cands)(using wasm))
          .`catch`[Unit](e => report(s"wasm stage failed: $e")): Unit
      case "dump" :: _           =>
        dumpStage(cfg, cands)
      case "wasm" :: _           =>
        report(
          s"$runtime  offsets=${cfg.offsets.mkString(",")} reps=${cfg.reps} candidates=${cfg.candidates}"
        )
        loadKernel()
          .`then`[Unit](wasm => wasmStage(cfg, cands, wasm))
          .`catch`[Unit](e => report(s"wasm stage failed: $e")): Unit
      case _                     =>
        report(
          s"$runtime  offsets=${cfg.offsets.mkString(",")} reps=${cfg.reps} candidates=${cfg.candidates}"
        )
        jtsStage(cfg, cands)
