// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import cats.data.NonEmptyList
import lucuma.core.geom.Area
import lucuma.core.geom.BoundingOffsets
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapePolygon
import lucuma.core.math.Angle
import lucuma.core.math.Offset

import scala.scalajs.js.typedarray.Float64Array

/**
 * A `Shape` backed by a geometry in the wasm kernel's arena. The handle is released when the
 * enclosing `WasmShapeInterpreter.withArena` block ends, by `free()`, or by the JS garbage
 * collector if the shape was created outside any arena. Using a released shape throws.
 */
final class WasmShape private[wasm] (private[wasm] val handle: Int) extends Shape:

  private var released: Boolean = false

  // Set when the garbage-collector cleanup owns the handle, i.e. it was created outside any arena.
  private[wasm] var registered: Boolean = false

  private[wasm] def isReleased: Boolean = released

  private def h: Int =
    if (released)
      throw new IllegalStateException(
        s"WasmShape (handle $handle) used after release; shapes created inside withArena must not escape it"
      )
    else handle

  /** Releases the kernel geometry. Idempotent. */
  def free(): Unit =
    if (!released) {
      released = true
      WasmShapeInterpreter.release(this)
    }

  // Shapes never change, so the kernel is asked for the box once; the AGS loop reads it per star.
  private lazy val bbox: Float64Array = LucumaWasm.bbox(h)

  lazy val boundingOffsets: BoundingOffsets =
    if (bbox(0).isNaN) BoundingOffsets(Offset.Zero, Offset.Zero)
    else
      BoundingOffsets(
        WasmCoords.toOffset(bbox(0), bbox(3)),
        WasmCoords.toOffset(bbox(2), bbox(1))
      )

  def contains(o: Offset): Boolean =
    LucumaWasm.contains_point(h, WasmCoords.x(o), WasmCoords.y(o))

  def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(LucumaWasm.area(h).round).getOrElse(Area.MinArea)

  // Distance to the vertex farthest from the origin, as JTS does. `coords` has exterior rings
  // only, which is enough: a hole lies inside its polygon, so its vertices are never farthest.
  def radius: Angle =
    val cs = LucumaWasm.coords(h)
    (0 until cs.length by 2)
      .maxByOption(i => cs(i) * cs(i) + cs(i + 1) * cs(i + 1))
      .fold(Angle.Angle0)(i => WasmCoords.toOffset(cs(i), cs(i + 1)).distance(Offset.Zero))

  lazy val isEmpty: Boolean = bbox(0).isNaN

  // One affine call with the composed matrix; agrees with the three-step expression to the last
  // bits only, so tests compare with a tolerance.
  def transform(preTranslation: Offset, rotation: Angle, postTranslation: Offset): Shape = {
    val t  = rotation.toDoubleRadians
    val c  = math.cos(t)
    val s  = math.sin(t)
    val px = WasmCoords.x(preTranslation)
    val py = WasmCoords.y(preTranslation)
    val qx = WasmCoords.x(postTranslation)
    val qy = WasmCoords.y(postTranslation)
    WasmShapeInterpreter.wrap(
      LucumaWasm.affine(h, c, -s, c * px - s * py + qx, s, c, s * px + c * py + qy)
    )
  }

  def intersects(that: Shape): Boolean =
    that match
      case w: WasmShape => LucumaWasm.intersects(h, w.h)
      case _            => throw WasmShape.mixed(that)

  def intersection(that: Shape): Shape =
    that match
      case w: WasmShape => WasmShapeInterpreter.wrap(LucumaWasm.op(0, h, w.h))
      case _            => throw WasmShape.mixed(that)

  // Parses the kernel's `rings` layout; each reader returns its value and the index after it.
  def polygons: List[ShapePolygon] =
    val r = LucumaWasm.rings(h)

    def many[A](n: Int, at: Int)(read: Int => (A, Int)): (List[A], Int) =
      val (as, end) = (0 until n).foldLeft((List.empty[A], at)):
        case ((acc, i), _) =>
          val (a, next) = read(i)
          (a :: acc, next)
      (as.reverse, end)

    def ring(at: Int): (List[Offset], Int) =
      val n = r(at).toInt
      (List.tabulate(n)(k => WasmCoords.toOffset(r(at + 1 + 2 * k), r(at + 2 + 2 * k))),
       at + 1 + 2 * n
      )

    // A polygon whose exterior ring came back empty is dropped rather than drawn.
    def polygon(at: Int): (Option[ShapePolygon], Int) =
      val (rings, end) = many(r(at).toInt, at + 1)(ring)
      val polygon      = rings match
        case exterior :: holes =>
          NonEmptyList
            .fromList(exterior)
            .map(ShapePolygon(_, holes.flatMap(NonEmptyList.fromList)))
        case Nil               => None
      (polygon, end)

    many(r(0).toInt, 1)(polygon)._1.flatten

  override def toString: String =
    if (released) s"WasmShape($handle, released)" else s"WasmShape($handle)"

object WasmShape:
  private[wasm] def mixed(that: Shape): UnsupportedOperationException =
    new UnsupportedOperationException(
      s"Cannot combine ${that.getClass.getSimpleName} with WasmShape: shapes from different engines never mix"
    )

/** µas coordinate convention shared with `lucuma.core.geom.jts`: x = -p, y = q. */
private[wasm] object WasmCoords:
  inline def x(o: Offset): Double = -Angle.signedMicroarcseconds.get(o.p.toAngle).toDouble
  inline def y(o: Offset): Double = Angle.signedMicroarcseconds.get(o.q.toAngle).toDouble

  // Truncates to whole µas, as JTS does, so both engines report the same offsets.
  def toOffset(x: Double, y: Double): Offset =
    Offset(
      Offset.P(Angle.fromMicroarcseconds(-x.toLong)),
      Offset.Q(Angle.fromMicroarcseconds(y.toLong))
    )
