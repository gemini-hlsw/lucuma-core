// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.effect.unsafe.IORuntime
import lucuma.ags.*
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.jts.JtsShapeInterpreter
import lucuma.core.geom.wasm.WasmGeometry
import lucuma.core.geom.wasm.WasmShapeInterpreter
import lucuma.core.math.Angle
import org.locationtech.jts.geom.GeometryOverlay

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

// Node benchmark of the AGS hot path on the workloads in `AgsWorkload`. Arguments: workloads
// (`real`, or N for an N-point offset grid), then any of `wasm` (pair JTS with the kernel),
// `dump` (one line per analysis instead of timings) and `old` (legacy JTS overlay).
object AgsBench:

  private val Reps = 3

  def ms(nanos: Long): String = f"${nanos / 1.0e6}%.1f"

  def argv: List[String] = global.process.argv.asInstanceOf[js.Array[String]].toList.drop(2)

  def dumpLine(a: AgsAnalysis): String =
    val vig = a match
      case u: AgsAnalysis.Usable => u.vignetting.toMicroarcsecondsSquared.toString
      case _                     => "-"
    s"${Ags.resultLabel(a)}\t${a.target.id}\t${Angle.microarcseconds.get(a.posAngle)}\t$vig"

  def histogram(s: AgsStats): String =
    s"accepted=${s.acceptedCount} " + s.resultCounts.toList
      .sortBy(_._1)
      .map((k, v) => s"$k=$v")
      .mkString(" ")

  // Node cannot fetch the package's own file: URL; hand the loader the bytes.
  def loadKernel(): js.Promise[ShapeInterpreter] =
    given IORuntime = cats.effect.unsafe.implicits.global
    js.`import`[js.Dynamic]("node:fs")
      .`then`[ShapeInterpreter]: fs =>
        val url   = js.`import`.meta
          .asInstanceOf[js.Dynamic]
          .resolve("@gemini-hlsw/lucuma-wasm/lucuma_wasm_bg.wasm")
        val bytes = fs.readFileSync(js.Dynamic.newInstance(global.URL)(url)).asInstanceOf[js.Any]
        WasmGeometry.loadFrom(bytes).unsafeToPromise()

  // Times each workload on every engine, paired per rep, then compares the engines' outcomes.
  def timeStage(workloads: List[AgsWorkload], engines: List[(String, ShapeInterpreter)]): Unit =
    val kernel    = engines.size > 1
    def live: Int = if kernel then WasmShapeInterpreter.liveHandles else 0
    println(
      "workload\tengine\trep\tcalcs_ms\tcontext_ms\tanalysis_ms\ttotal_ms\tlive_handles\twasm_mb"
    )
    engines.foreach((_, si) => workloads.head.run(using si))
    workloads.foreach: w =>
      val stats    = (1 to Reps).toList.flatMap: rep =>
        engines.map: (name, si) =>
          val before = live
          val s      = w.run(using si).stats
          val mb     = f"${WasmShapeInterpreter.memoryBytes / 1048576.0}%.1f"
          println(
            s"${w.name}\t$name\t$rep\t${ms(s.calcsNanos)}\t${ms(s.contextNanos)}\t${ms(s.analysisNanos)}\t${ms(s.contextNanos + s.analysisNanos)}\t${live - before}\t$mb"
          )
          name -> s
      val byEngine = engines.map((name, _) => name -> stats.collect { case (`name`, s) => s })
      val avgs     = byEngine.map: (name, ss) =>
        val c = ss.map(_.calcsNanos).sum / ss.size
        val a = ss.map(_.analysisNanos).sum / ss.size
        println(s"${w.name}\t$name\tavg\tcalcs=${ms(c)} analysis=${ms(a)} total=${ms(c + a)}")
        (c, a)
      avgs match
        case List((jc, ja), (wc, wa)) =>
          println(
            f"${w.name}\tspeedup\tcalcs=${jc.toDouble / wc}%.2fx analysis=${ja.toDouble / wa}%.2fx total=${(jc + ja).toDouble / (wc + wa)}%.2fx"
          )
        case _                        => ()
      val hists    = byEngine.map((name, ss) => name -> histogram(ss.last))
      hists.foreach((name, h) => println(s"${w.name}\thistogram\t$name\t$h"))
      if hists.map(_._2).distinct.size > 1 then
        println(s"${w.name}\tHISTOGRAM MISMATCH between engines")

  def main(args: Array[String]): Unit =
    val names     = argv.headOption.fold(List("real"))(_.split(",").toList.map(_.trim))
    val workloads =
      names.map(n => AgsWorkload.named(n).getOrElse(sys.error(s"unknown workload $n")))
    val mode      = argv.drop(1)
    if mode.contains("old") then
      GeometryOverlay.OVERLAY_NG_DEFAULT = false
      GeometryOverlay.setOverlayImpl("old")
      println("jts overlay: legacy")
    val kernel    =
      if mode.contains("wasm") then loadKernel().`then`[Option[ShapeInterpreter]](Some(_))
      else js.Promise.resolve[Option[ShapeInterpreter]](None)
    kernel
      .`then`[Unit]: k =>
        if mode.contains("dump") then
          val si = k.getOrElse(JtsShapeInterpreter)
          workloads.head.run(using si).analyses.foreach(a => println(dumpLine(a)))
        else
          println(
            s"node ${global.process.versions.node}  workloads=${names.mkString(",")} reps=$Reps"
          )
          timeStage(workloads, ("jts" -> JtsShapeInterpreter) :: k.map("wasm" -> _).toList)
      .`catch`[Unit](e => println(s"failed: $e")): Unit
