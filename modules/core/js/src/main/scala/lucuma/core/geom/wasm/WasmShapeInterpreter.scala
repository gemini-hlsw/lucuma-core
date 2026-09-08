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
 * it goes, so only the shapes handed back to callers occupy the arena.
 *
 * Memory: run computations inside `scoped`, which frees every shape created within it on exit
 * (nesting allowed). Shapes created outside any scope are freed when the JS garbage collector
 * collects the wrapper, via `FinalizationRegistry`, or eagerly with `WasmShape.free()`.
 *
 * Obtain it through `WasmGeometry.load`; the kernel must be initialised before use.
 */
object WasmShapeInterpreter extends ShapeInterpreter {

  // JTS GeometricShapeFactory default; keeps ellipses and arcs vertex-for-vertex with JTS.
  private val NPts = 100

  private var loaded: Boolean = false

  private var scopes: List[ArrayBuffer[WasmShape]] = Nil

  private val registry: js.FinalizationRegistry[WasmShape, Int, WasmShape] =
    new js.FinalizationRegistry(h => LucumaGeoWasm.free(h))

  private[wasm] def markLoaded(): Unit = loaded = true

  /** Number of geometries currently held by the kernel; a leak detector for tests. */
  def liveHandles: Int = LucumaGeoWasm.live()

  /** True while at least one `scoped` block is running. */
  def inScope: Boolean = scopes.nonEmpty

  override def scoped[A](f: => A): A = {
    val arena = ArrayBuffer.empty[WasmShape]
    scopes = arena :: scopes
    try f
    finally {
      scopes = scopes.tail
      arena.foreach(_.free())
    }
  }

  private[wasm] def wrap(h: Int): WasmShape = {
    val s = new WasmShape(h)
    scopes match {
      case arena :: _ => arena += s
      case Nil        => registry.register(s, h, s)
    }
    s
  }

  private[wasm] def release(s: WasmShape): Unit = {
    registry.unregister(s)
    LucumaGeoWasm.free(s.handle)
  }

  private def rectBounded(a: Offset, b: Offset)(f: (Double, Double, Double, Double) => Int): Int =
    if (a.p === b.p || a.q === b.q) LucumaGeoWasm.empty_new()
    else f(WasmCoords.x(a), WasmCoords.y(a), WasmCoords.x(b), WasmCoords.y(b))

  private def polygon(os: List[Offset]): Int =
    if (os.toSet.size < 3) LucumaGeoWasm.empty_new()
    else {
      val os2 = if (os.head === os.last) os else os.last :: os
      val arr = new Float64Array(os2.size * 2)
      var i   = 0
      os2.foreach { o =>
        arr(i) = WasmCoords.x(o)
        arr(i + 1) = WasmCoords.y(o)
        i += 2
      }
      LucumaGeoWasm.poly_new(arr)
    }

  private def combine(kind: Int, a: ShapeExpression, b: ShapeExpression): Int = {
    val ha = go(a)
    val hb =
      try go(b)
      catch { case t: Throwable => LucumaGeoWasm.free(ha); throw t }
    try LucumaGeoWasm.op(kind, ha, hb)
    finally {
      LucumaGeoWasm.free(ha)
      LucumaGeoWasm.free(hb)
    }
  }

  private def transform(
    e:   ShapeExpression,
    m00: Double,
    m01: Double,
    m02: Double,
    m10: Double,
    m11: Double,
    m12: Double
  ): Int = {
    val h = go(e)
    try LucumaGeoWasm.affine(h, m00, m01, m02, m10, m11, m12)
    finally LucumaGeoWasm.free(h)
  }

  private def go(e: ShapeExpression): Int = e match {
    // Constructors
    case Empty                 => LucumaGeoWasm.empty_new()
    case Ellipse(a, b)         => rectBounded(a, b)(LucumaGeoWasm.ellipse_new(_, _, _, _, NPts))
    case ClosedArc(a, b, c, d) =>
      rectBounded(a, b)(LucumaGeoWasm.arc_new(_, _, _, _, c.toDoubleRadians, d.toDoubleRadians, NPts))
    case Polygon(os)           => polygon(os)
    case Rectangle(a, b)       => rectBounded(a, b)(LucumaGeoWasm.rect_new)
    // The kernel has no zero-area point type; JTS's point is empty for every area/overlay purpose.
    case Point(_)              => LucumaGeoWasm.empty_new()
    case BoundingBox(e)        =>
      val h = go(e)
      val b =
        try LucumaGeoWasm.bbox(h)
        finally LucumaGeoWasm.free(h)
      if (b(0).isNaN || b(0) == b(2) || b(1) == b(3)) LucumaGeoWasm.empty_new()
      else LucumaGeoWasm.rect_new(b(0), b(1), b(2), b(3))

    // Combinations
    case Difference(a, b)   => combine(2, a, b)
    case Intersection(a, b) => combine(0, a, b)
    case Union(a, b)        => combine(1, a, b)

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
  }

  override def interpret(e: ShapeExpression): Shape = {
    if (!loaded)
      throw new IllegalStateException("lucuma-geo-wasm is not loaded; run WasmGeometry.load first")
    wrap(go(e))
  }
}
