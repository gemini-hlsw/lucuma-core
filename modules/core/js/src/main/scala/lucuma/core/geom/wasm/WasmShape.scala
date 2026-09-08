// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import lucuma.core.geom.Area
import lucuma.core.geom.BoundingOffsets
import lucuma.core.geom.Shape
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * A `Shape` backed by a geometry in the wasm kernel's arena. The handle is released when the
 * enclosing `WasmShapeInterpreter.scoped` block ends, by `free()`, or by the JS garbage collector
 * if the shape was created outside any scope. Using a released shape throws.
 */
final class WasmShape private[wasm] (private[wasm] val handle: Int) extends Shape {

  private var released: Boolean = false

  private[wasm] def isReleased: Boolean = released

  private def h: Int =
    if (released)
      throw new IllegalStateException(
        s"WasmShape (handle $handle) used after release; shapes created inside scoped must not escape it"
      )
    else handle

  /** Releases the kernel geometry. Idempotent. */
  def free(): Unit =
    if (!released) {
      released = true
      WasmShapeInterpreter.release(this)
    }

  def boundingOffsets: BoundingOffsets = {
    val b = LucumaGeoWasm.bbox(h)
    if (b(0).isNaN) BoundingOffsets(Offset.Zero, Offset.Zero)
    else
      BoundingOffsets(
        WasmCoords.toOffset(b(0), b(3)),
        WasmCoords.toOffset(b(2), b(1))
      )
  }

  def contains(o: Offset): Boolean =
    LucumaGeoWasm.contains_point(h, WasmCoords.x(o), WasmCoords.y(o))

  def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(LucumaGeoWasm.area(h).round).getOrElse(Area.MinArea)

  def radius: Angle = {
    val cs   = LucumaGeoWasm.coords(h)
    var best = -1.0
    var bx   = 0.0
    var by   = 0.0
    var i    = 0
    while (i < cs.length) {
      val x = cs(i)
      val y = cs(i + 1)
      val d = x * x + y * y
      if (d > best) { best = d; bx = x; by = y }
      i += 2
    }
    if (best < 0) Angle.Angle0
    else WasmCoords.toOffset(bx, by).distance(Offset.Zero)
  }

  def intersects(that: Shape): Boolean = that match {
    case w: WasmShape => LucumaGeoWasm.intersects(h, w.h)
    case _            => throw WasmShape.mixed(that)
  }

  def intersection(that: Shape): Shape = that match {
    case w: WasmShape => WasmShapeInterpreter.wrap(LucumaGeoWasm.op(0, h, w.h))
    case _            => throw WasmShape.mixed(that)
  }

  override def toString: String =
    if (released) s"WasmShape($handle, released)" else s"WasmShape($handle)"
}

object WasmShape {
  private[wasm] def mixed(that: Shape): UnsupportedOperationException =
    new UnsupportedOperationException(
      s"Cannot combine ${that.getClass.getSimpleName} with WasmShape: shapes from different engines never mix"
    )
}

/** µas coordinate convention shared with `lucuma.core.geom.jts`: x = -p, y = q. */
private[wasm] object WasmCoords {
  inline def x(o: Offset): Double = -Angle.signedMicroarcseconds.get(o.p.toAngle).toDouble
  inline def y(o: Offset): Double = Angle.signedMicroarcseconds.get(o.q.toAngle).toDouble

  def toOffset(x: Double, y: Double): Offset =
    Offset(
      Offset.P(Angle.fromMicroarcseconds(-x.round)),
      Offset.Q(Angle.fromMicroarcseconds(y.round))
    )
}
