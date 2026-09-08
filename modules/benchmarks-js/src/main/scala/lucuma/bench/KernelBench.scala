// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.syntax.all.*
import lucuma.ags.Ags
import lucuma.ags.AgsParams
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.jts.JtsShapeInterpreter
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * PROTOTYPE: the posCalculations per-posAngle overlay chain, evaluated by JTS (Scala.js) and by the
 * Rust geo kernel (wasm) in the same process. Times both and checks parity.
 */
object KernelBench:

  final case class Result(nanos: Long, areas: List[Long], bboxes: List[String], shapes: List[Shape])

  private val params = AgsBench.params.asInstanceOf[AgsParams.GmosImaging]

  def positions(offsets: Int) = Ags
    .generatePositions(AgsBench.base.some, None, AgsBench.posAngles, None, AgsBench.scienceOffsets(offsets))
    .value
    .toNonEmptyList

  def chains(offsets: Int): List[(Angle, ShapeExpression)] =
    val distinct = positions(offsets).map(p => (p.offsetPos, p.pivot)).distinct
    AgsBench.posAngles.toList.map: pa =>
      pa -> distinct.map((o, pivot) => params.patrolFieldAt(pa, o, pivot)).reduce(using _ ∩ _)

  // The other half of posCalculations: one science-area eval per position
  def scienceAreas(offsets: Int): List[(Angle, ShapeExpression)] =
    positions(offsets).toList.map(p => p.posAngle -> params.scienceArea(p.posAngle, p.offsetPos))

  def evalAll(cs: List[(Angle, ShapeExpression)], interp: ShapeInterpreter): Result =
    val t0     = System.nanoTime()
    val shapes = cs.map((_, e) => e.eval(using interp))
    val nanos  = System.nanoTime() - t0
    Result(
      nanos,
      shapes.map(s => lucuma.core.geom.Area.fromMicroarcsecondsSquared.reverseGet(s.area)),
      shapes.map(s => s"${s.boundingOffsets.topLeft.p.toAngle.toMicroarcseconds}:${s.boundingOffsets.topLeft.q.toAngle.toMicroarcseconds}:${s.boundingOffsets.bottomRight.p.toAngle.toMicroarcseconds}:${s.boundingOffsets.bottomRight.q.toAngle.toMicroarcseconds}"),
      shapes
    )

  def run(offsets: Int, reps: Int, geo: GeoWasmInterpreter, cands: List[Offset])(using AgsGeoModule): List[String] =
    val cs   = chains(offsets)
    val jts  = (1 to reps).toList.map(_ => evalAll(cs, JtsShapeInterpreter))
    val wasm = (1 to reps).toList.map(_ => evalAll(cs, geo))
    val ms   = (r: List[Result]) => f"${r.map(_.nanos).sum / r.size / 1.0e6}%.1f"

    val j = jts.last
    val w = wasm.last
    val areaRelDiff = j.areas.zip(w.areas).map((a, b) => if a == 0 then (if b == 0 then 0.0 else 1.0) else math.abs(a - b).toDouble / a)
    val bboxMismatch = j.bboxes.zip(w.bboxes).count(_ != _)
    val bboxMaxDiff  = j.shapes.zip(w.shapes).map: (sj, sw) =>
      val a = sj.boundingOffsets; val b = sw.boundingOffsets
      List(a.topLeft.p.toAngle.toMicroarcseconds - b.topLeft.p.toAngle.toMicroarcseconds,
           a.topLeft.q.toAngle.toMicroarcseconds - b.topLeft.q.toAngle.toMicroarcseconds,
           a.bottomRight.p.toAngle.toMicroarcseconds - b.bottomRight.p.toAngle.toMicroarcseconds,
           a.bottomRight.q.toAngle.toMicroarcseconds - b.bottomRight.q.toAngle.toMicroarcseconds).map(math.abs).max
    .max
    val containsChecks = for { (sj, sw) <- j.shapes.zip(w.shapes); c <- cands } yield (sj.contains(c), sw.contains(c))
    val containsMismatch = containsChecks.count((a, b) => a != b)
    val containsTrue     = containsChecks.count(_._1)
    // free wasm shapes we hold
    wasm.foreach(_.shapes.foreach { case s: GeoWasmShape => s.free(); case _ => () })

    val sa    = scienceAreas(offsets)
    val saJ   = (1 to reps).toList.map(_ => evalAll(sa, JtsShapeInterpreter))
    val saW   = (1 to reps).toList.map(_ => evalAll(sa, geo))
    val saRel = saJ.last.areas.zip(saW.last.areas).map((a, b) => if a == 0 then (if b == 0 then 0.0 else 1.0) else math.abs(a - b).toDouble / a).max
    saW.foreach(_.shapes.foreach { case s: GeoWasmShape => s.free(); case _ => () })
    val totJ  = jts.map(_.nanos).sum + saJ.map(_.nanos).sum
    val totW  = wasm.map(_.nanos).sum + saW.map(_.nanos).sum

    List(
      s"$offsets\tsciarea\tevals=${sa.size}\tjts_ms=${ms(saJ)}\twasm_ms=${ms(saW)}\tspeedup=${f"${saJ.map(_.nanos).sum.toDouble / saW.map(_.nanos).sum}%.2f"}x\tarea_max_rel_diff=${f"$saRel%.2e"}",
      s"$offsets\tcalcs_total\tjts_ms=${f"${totJ / reps / 1.0e6}%.1f"}\twasm_ms=${f"${totW / reps / 1.0e6}%.1f"}\tspeedup=${f"${totJ.toDouble / totW}%.2f"}x",
      s"$offsets\tkernel\tchains=${cs.size}\tjts_ms=${ms(jts)}\twasm_ms=${ms(wasm)}\tspeedup=${f"${jts.map(_.nanos).sum.toDouble / wasm.map(_.nanos).sum}%.2f"}x",
      s"$offsets\tparity\tarea_max_rel_diff=${f"${areaRelDiff.max}%.2e"}\tbbox_exact_mismatch=$bboxMismatch/${cs.size}\tbbox_max_diff_uas=$bboxMaxDiff\tcontains_mismatch=$containsMismatch/${containsChecks.size} (true=$containsTrue)\tlive_handles=${summon[AgsGeoModule].live()}"
    )
