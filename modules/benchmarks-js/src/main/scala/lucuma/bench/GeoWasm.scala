// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.syntax.all.*
import lucuma.core.geom.Area
import lucuma.core.geom.BoundingOffsets
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeExpression.*
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.jts.Jts
import lucuma.core.math.Angle
import lucuma.core.math.Offset

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.typedarray.Float64Array

// PROTOTYPE: Scala.js facade over the wasm-bindgen output of agsgeo (Rust geo crate).
@js.native
trait AgsGeoModule extends js.Object:
  def empty_new(): Int                                                                   = js.native
  def poly_new(coords: Float64Array): Int                                                = js.native
  def rect_new(x0: Double, y0: Double, x1: Double, y1: Double): Int                      = js.native
  def ellipse_new(x0: Double, y0: Double, x1: Double, y1: Double, npts: Int): Int        = js.native
  def arc_new(x0: Double, y0: Double, x1: Double, y1: Double, s: Double, e: Double, n: Int): Int =
    js.native
  def op(kind: Int, a: Int, b: Int): Int                                                 = js.native
  def affine(h: Int, m00: Double, m01: Double, m02: Double, m10: Double, m11: Double, m12: Double): Int =
    js.native
  def area(h: Int): Double                                                               = js.native
  def bbox(h: Int): Float64Array                                                         = js.native
  def contains_point(h: Int, x: Double, y: Double): Boolean                              = js.native
  def intersects(a: Int, b: Int): Boolean                                                = js.native
  def free(h: Int): Unit                                                                 = js.native
  def live(): Int                                                                        = js.native
  @JSName("default")
  def init(module: js.Any): js.Promise[js.Any]                                           = js.native

// Same µas coordinate convention as lucuma.core.geom.jts.syntax: x = -p, y = q
object GeoCoords:
  inline def x(o: Offset): Double = -Angle.signedMicroarcseconds.get(o.p.toAngle).toDouble
  inline def y(o: Offset): Double = Angle.signedMicroarcseconds.get(o.q.toAngle).toDouble

final case class GeoWasmShape(h: Int)(using m: AgsGeoModule) extends Shape:
  def boundingOffsets: BoundingOffsets =
    val b = m.bbox(h)
    if b(0).isNaN then BoundingOffsets(Offset.Zero, Offset.Zero)
    else BoundingOffsets(Jts.coord2offset(b(0), b(3)), Jts.coord2offset(b(2), b(1)))

  def contains(o: Offset): Boolean = m.contains_point(h, GeoCoords.x(o), GeoCoords.y(o))

  def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(m.area(h).round).getOrElse(Area.MinArea)

  def radius: Angle = throw new UnsupportedOperationException("radius not implemented in prototype")

  def intersects(that: Shape): Boolean = that match
    case GeoWasmShape(o) => m.intersects(h, o)
    case _               => throw new UnsupportedOperationException("mixed shapes")

  def intersection(that: Shape): Shape = that match
    case GeoWasmShape(o) => GeoWasmShape(m.op(0, h, o))
    case _               => throw new UnsupportedOperationException("mixed shapes")

  def free(): Unit = m.free(h)

/** Mirrors JtsShapeInterpreter, freeing intermediate handles as it goes. */
final class GeoWasmInterpreter(using m: AgsGeoModule) extends ShapeInterpreter:
  private val NPts = 100 // JTS GeometricShapeFactory default

  private def rectBounded(a: Offset, b: Offset)(f: (Double, Double, Double, Double) => Int): Int =
    if a.p === b.p || a.q === b.q then m.empty_new()
    else f(GeoCoords.x(a), GeoCoords.y(a), GeoCoords.x(b), GeoCoords.y(b))

  private def polygon(os: List[Offset]): Int =
    if os.toSet.size < 3 then m.empty_new()
    else
      val os2 = if os.head === os.last then os else os.last :: os
      val arr = new Float64Array(os2.size * 2)
      os2.zipWithIndex.foreach: (o, i) =>
        arr(2 * i) = GeoCoords.x(o)
        arr(2 * i + 1) = GeoCoords.y(o)
      m.poly_new(arr)

  private def combine(kind: Int, a: ShapeExpression, b: ShapeExpression): Int =
    val ha = go(a)
    val hb = go(b)
    val r  = m.op(kind, ha, hb)
    m.free(ha); m.free(hb)
    r

  private def transform(e: ShapeExpression, m00: Double, m01: Double, m02: Double, m10: Double, m11: Double, m12: Double): Int =
    val h = go(e)
    val r = m.affine(h, m00, m01, m02, m10, m11, m12)
    m.free(h)
    r

  private def go(e: ShapeExpression): Int = e match
    case Empty                 => m.empty_new()
    case Ellipse(a, b)         => rectBounded(a, b)(m.ellipse_new(_, _, _, _, NPts))
    case ClosedArc(a, b, c, d) =>
      rectBounded(a, b)(m.arc_new(_, _, _, _, c.toDoubleRadians, d.toDoubleRadians, NPts))
    case Polygon(os)           => polygon(os)
    case Rectangle(a, b)       => rectBounded(a, b)(m.rect_new)
    case Point(a)              => polygon(List(a)) // empty, as in JTS (zero-area point)
    case BoundingBox(e)        =>
      val h = go(e)
      val b = m.bbox(h)
      m.free(h)
      if b(0).isNaN then m.empty_new() else m.rect_new(b(0), b(1), b(2), b(3))

    case Difference(a, b)      => combine(2, a, b)
    case Intersection(a, b)    => combine(0, a, b)
    case Union(a, b)           => combine(1, a, b)

    case FlipP(e)                    => transform(e, -1, 0, 0, 0, 1, 0)
    case FlipQ(e)                    => transform(e, 1, 0, 0, 0, -1, 0)
    case Rotate(e, a)                =>
      val t = a.toDoubleRadians
      transform(e, math.cos(t), -math.sin(t), 0, math.sin(t), math.cos(t), 0)
    case RotateAroundOffset(e, a, o) =>
      val t  = a.toDoubleRadians
      val cx = GeoCoords.x(o)
      val cy = GeoCoords.y(o)
      val c  = math.cos(t)
      val s  = math.sin(t)
      transform(e, c, -s, cx - cx * c + cy * s, s, c, cy - cx * s - cy * c)
    case Translate(e, o)             => transform(e, 1, 0, GeoCoords.x(o), 0, 1, GeoCoords.y(o))

  override def interpret(e: ShapeExpression): Shape = GeoWasmShape(go(e))
