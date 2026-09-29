// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import cats.syntax.all.*
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeExpression.*
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.math.Offset

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.typedarray.Float64Array

/**
 * `ShapeInterpreter` over the wasm kernel. Mirrors `JtsShapeInterpreter` case by case (100-point
 * ellipse and arc discretisation, empty-geometry guards) and frees every intermediate handle as
 * it goes, so only the shapes handed back to callers occupy kernel memory.
 *
 * Memory: run computations inside `withArena`, which frees every shape created within it on exit
 * (nesting allowed). Shapes created outside any arena are freed when the JS garbage collector
 * collects the wrapper, via `FinalizationRegistry`, or eagerly with `WasmShape.free()`.
 *
 * Obtain it through `WasmGeometry.load`; the kernel must be initialised before use.
 */
object WasmShapeInterpreter extends ShapeInterpreter:

  // JTS GeometricShapeFactory default; keeps ellipses and arcs vertex-for-vertex with JTS.
  private val NPts = 100

  private var loaded: Boolean = false

  private var arenas: List[ArrayBuffer[WasmShape]] = Nil

  private val registry: js.FinalizationRegistry[WasmShape, Int, WasmShape] =
    new js.FinalizationRegistry(h => LucumaWasm.free(h))

  private var exports: js.Dynamic = null

  private[wasm] def markLoaded(wasmExports: js.Any): Unit =
    exports = wasmExports.asInstanceOf[js.Dynamic]
    loaded = true

  /** Number of geometries currently held by the kernel; a leak detector for tests. */
  def liveHandles: Int = LucumaWasm.live()

  /**
   * Bytes of wasm linear memory currently reserved by the kernel. Linear memory grows on demand
   * and never shrinks, so this is the peak working set since load; 0 before `WasmGeometry.load`.
   */
  def memoryBytes: Long =
    if (exports == null) 0L
    else exports.memory.buffer.byteLength.asInstanceOf[Double].toLong

  /** True while at least one `withArena` block is running. */
  def inArena: Boolean = arenas.nonEmpty

  override def withArena[A](f: => A): A =
    val arena = ArrayBuffer.empty[WasmShape]
    arenas = arena :: arenas
    try f
    finally {
      arenas = arenas.tail
      arena.foreach(_.free())
    }

  private[wasm] def wrap(h: Int): WasmShape =
    val s = new WasmShape(h)
    arenas match {
      case arena :: _ => arena += s
      case Nil        => registry.register(s, h, s)
    }
    s

  private[wasm] def release(s: WasmShape): Unit =
    registry.unregister(s)
    LucumaWasm.free(s.handle)

  private def rectBounded(a: Offset, b: Offset)(f: (Double, Double, Double, Double) => Int): Int =
    if (a.p === b.p || a.q === b.q) LucumaWasm.empty_new()
    else f(WasmCoords.x(a), WasmCoords.y(a), WasmCoords.x(b), WasmCoords.y(b))

  private def polygon(os: List[Offset]): Int =
    if (os.toSet.size < 3) LucumaWasm.empty_new()
    else {
      val os2 = if (os.head === os.last) os else os.last :: os
      val arr = new Float64Array(os2.size * 2)
      var i   = 0
      os2.foreach { o =>
        arr(i) = WasmCoords.x(o)
        arr(i + 1) = WasmCoords.y(o)
        i += 2
      }
      LucumaWasm.poly_new(arr)
    }

  // Folds a chain of same-kind nodes in a loop, see ShapeExpression.leftSpine. Intermediates
  // are freed as soon as they are consumed.
  private def chain(op: BinaryOp, kind: Int, e: ShapeExpression): Int = {
    val (head, rest) = ShapeExpression.leftSpine(e, op)
    var acc          = go(head)
    rest.foreach { x =>
      val hx   =
        try go(x)
        catch { case t: Throwable => LucumaWasm.free(acc); throw t }
      val next =
        try LucumaWasm.op(kind, acc, hx)
        finally {
          LucumaWasm.free(acc)
          LucumaWasm.free(hx)
        }
      acc = next
    }
    acc
  }

  private def transform(
    e:   ShapeExpression,
    m00: Double,
    m01: Double,
    m02: Double,
    m10: Double,
    m11: Double,
    m12: Double
  ): Int =
    val h = go(e)
    try LucumaWasm.affine(h, m00, m01, m02, m10, m11, m12)
    finally LucumaWasm.free(h)

  private def go(e: ShapeExpression): Int = e match
    // Constructors
    case Empty                 => LucumaWasm.empty_new()
    case Ellipse(a, b)         => rectBounded(a, b)(LucumaWasm.ellipse_new(_, _, _, _, NPts))
    case ClosedArc(a, b, c, d) =>
      rectBounded(a, b)(LucumaWasm.arc_new(_, _, _, _, c.toDoubleRadians, d.toDoubleRadians, NPts))
    case Polygon(os)           => polygon(os)
    case Rectangle(a, b)       => rectBounded(a, b)(LucumaWasm.rect_new)
    // The kernel has no zero-area point type; JTS's point is empty for every area/overlay purpose.
    case Point(_)              => LucumaWasm.empty_new()
    case BoundingBox(e)        =>
      val h = go(e)
      val b =
        try LucumaWasm.bbox(h)
        finally LucumaWasm.free(h)
      if (b(0).isNaN || b(0) == b(2) || b(1) == b(3)) LucumaWasm.empty_new()
      else LucumaWasm.rect_new(b(0), b(1), b(2), b(3))

    // Combinations
    case Difference(_, _)   => chain(BinaryOp.Difference, 2, e)
    case Intersection(_, _) => chain(BinaryOp.Intersection, 0, e)
    case Union(_, _)        => chain(BinaryOp.Union, 1, e)

    // Transformations
    case FlipP(e)                    => transform(e, -1, 0, 0, 0, 1, 0)
    case FlipQ(e)                    => transform(e, 1, 0, 0, 0, -1, 0)
    case Rotate(e, a)                =>
      val t = a.toDoubleRadians
      val c = math.cos(t)
      val s = math.sin(t)
      transform(e, c, -s, 0, s, c, 0)
    case RotateAroundOffset(e, a, o) =>
      val t  = a.toDoubleRadians
      val c  = math.cos(t)
      val s  = math.sin(t)
      val cx = WasmCoords.x(o)
      val cy = WasmCoords.y(o)
      transform(e, c, -s, cx - cx * c + cy * s, s, c, cy - cx * s - cy * c)
    case Translate(e, o)             => transform(e, 1, 0, WasmCoords.x(o), 0, 1, WasmCoords.y(o))

  override def interpret(e: ShapeExpression): Shape =
    if (!loaded)
      throw new IllegalStateException("lucuma-wasm is not loaded; run WasmGeometry.load first")
    wrap(go(e))
