// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import cats.effect.IO
import cats.effect.Resource
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeExpression.*
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.jts.JtsShapeInterpreter
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import munit.CatsEffectSuite

import scala.scalajs.js

/**
 * Smoke tests for the kernel facade against JTS. The exhaustive cross-engine parity suite over
 * every AgsParams variant is `lucuma.ags.AgsGeometryParitySuite`.
 */
class WasmShapeInterpreterSuite extends CatsEffectSuite {

  private val kernel = ResourceSuiteLocalFixture(
    "kernel",
    Resource.make(WasmKernel.load)(_ =>
      IO(ShapeInterpreter.default = JtsShapeInterpreter)
    )
  )

  override def munitFixtures = List(kernel)

  private def µas(p: Long, q: Long): Offset =
    Offset(Offset.P(Angle.fromMicroarcseconds(p)), Offset.Q(Angle.fromMicroarcseconds(q)))

  private val Arcsec = 1_000_000L

  private val rect: ShapeExpression =
    Rectangle(µas(-10 * Arcsec, -5 * Arcsec), µas(10 * Arcsec, 5 * Arcsec))

  private val ellipse: ShapeExpression =
    Ellipse(µas(-8 * Arcsec, -3 * Arcsec), µas(4 * Arcsec, 9 * Arcsec))

  private val arc: ShapeExpression =
    ClosedArc(µas(-6 * Arcsec, -6 * Arcsec), µas(6 * Arcsec, 6 * Arcsec), Angle.Angle0, Angle.Angle90)

  private val overlay: ShapeExpression =
    Intersection(
      Rotate(rect, Angle.fromDoubleDegrees(37.5)),
      Union(Translate(ellipse, µas(2 * Arcsec, -1 * Arcsec)), Difference(rect, arc))
    )

  private def jts(e: ShapeExpression): Shape = JtsShapeInterpreter.interpret(e)

  private def assertClose(actual: Double, expected: Double, rel: Double, clue: String): Unit = {
    val tol = math.max(math.abs(expected) * rel, 1.0)
    assert(math.abs(actual - expected) <= tol, s"$clue: $actual vs $expected (tol $tol)")
  }

  private def assertAngleClose(a: Angle, b: Angle, µasTol: Long, clue: String): Unit = {
    val d = Angle.signedMicroarcseconds.get(a - b).abs
    assert(d <= µasTol, s"$clue: differ by $d µas")
  }

  private def assertParity(e: ShapeExpression, clue: String): Unit = {
    val w = kernel().interpret(e)
    val j = jts(e)
    assertClose(w.area.toMicroarcsecondsSquared.toDouble, j.area.toMicroarcsecondsSquared.toDouble, 1e-7, s"$clue area")
    assertAngleClose(w.radius, j.radius, 10, s"$clue radius")
    val wb = w.boundingOffsets
    val jb = j.boundingOffsets
    assertAngleClose(wb.topLeft.p.toAngle, jb.topLeft.p.toAngle, 10, s"$clue bbox tl.p")
    assertAngleClose(wb.topLeft.q.toAngle, jb.topLeft.q.toAngle, 10, s"$clue bbox tl.q")
    assertAngleClose(wb.bottomRight.p.toAngle, jb.bottomRight.p.toAngle, 10, s"$clue bbox br.p")
    assertAngleClose(wb.bottomRight.q.toAngle, jb.bottomRight.q.toAngle, 10, s"$clue bbox br.q")
    for {
      p <- -12 to 12
      q <- -12 to 12
    } {
      val o = µas(p * Arcsec + 333_333, q * Arcsec - 777_777)
      assertEquals(w.contains(o), j.contains(o), s"$clue contains $o")
    }
  }

  test("load installs the kernel as the default interpreter") {
    assertEquals(kernel(), WasmShapeInterpreter: ShapeInterpreter)
    assertEquals(ShapeInterpreter.default, WasmShapeInterpreter: ShapeInterpreter)
    assert(WasmGeometry.isCompatible(LucumaGeoWasm.version()), LucumaGeoWasm.version())
  }

  test("version range check") {
    assert(WasmGeometry.isCompatible("0.1.0"))
    assert(WasmGeometry.isCompatible("0.1.7-rc1"))
    assert(!WasmGeometry.isCompatible("0.2.0"))
    assert(!WasmGeometry.isCompatible("1.0.0"))
    assert(!WasmGeometry.isCompatible("garbage"))
  }

  test("rectangle matches JTS") {
    assertParity(rect, "rect")
  }

  test("ellipse matches JTS") {
    assertParity(ellipse, "ellipse")
  }

  test("closed arc matches JTS") {
    assertParity(arc, "arc")
  }

  test("polygon and flips match JTS") {
    val poly = Polygon(List(µas(0, 0), µas(7 * Arcsec, 1 * Arcsec), µas(3 * Arcsec, 9 * Arcsec), µas(-4 * Arcsec, 2 * Arcsec)))
    assertParity(poly, "poly")
    assertParity(FlipP(poly), "flipP")
    assertParity(FlipQ(poly), "flipQ")
    assertParity(RotateAroundOffset(poly, Angle.fromDoubleDegrees(210), µas(1 * Arcsec, 1 * Arcsec)), "rotateAround")
  }

  test("overlay chain matches JTS") {
    assertParity(overlay, "overlay")
    assertParity(BoundingBox(overlay), "bbox(overlay)")
  }

  test("empty and degenerate shapes") {
    val w = kernel()
    for (e <- List(Empty, Point(µas(1, 2)), Rectangle(µas(0, 0), µas(0, 5 * Arcsec)), Polygon(List(µas(0, 0), µas(1, 1))))) {
      val s = w.interpret(e)
      assertEquals(s.area.toMicroarcsecondsSquared, 0L, e.toString)
      assert(!s.contains(µas(0, 0)), e.toString)
      assertEquals(s.radius, Angle.Angle0, e.toString)
    }
  }

  test("Shape.intersects and intersection agree with JTS") {
    val w  = kernel()
    val a  = Rotate(rect, Angle.fromDoubleDegrees(20))
    val b  = Translate(ellipse, µas(6 * Arcsec, 0))
    val wa = w.interpret(a)
    val wb = w.interpret(b)
    val ja = jts(a)
    val jb = jts(b)
    assertEquals(wa.intersects(wb), ja.intersects(jb))
    assertClose(
      wa.intersection(wb).area.toMicroarcsecondsSquared.toDouble,
      ja.intersection(jb).area.toMicroarcsecondsSquared.toDouble,
      1e-7,
      "intersection area"
    )
    val far = w.interpret(Translate(rect, µas(100 * Arcsec, 0)))
    assert(!wa.intersects(far))
  }

  test("engines never mix") {
    val w = kernel().interpret(rect)
    val j = jts(rect)
    intercept[UnsupportedOperationException](w.intersects(j))
    intercept[UnsupportedOperationException](w.intersection(j))
    intercept[UnsupportedOperationException](j.intersects(w))
  }

  test("scoped frees every shape created inside, nested scopes included") {
    val w      = kernel()
    val before = WasmShapeInterpreter.liveHandles
    var leaked: Shape = null
    val area = w.scoped {
      val s1 = w.interpret(overlay)
      val a2 = w.scoped(w.interpret(rect).intersection(w.interpret(ellipse)).area)
      leaked = s1
      assert(WasmShapeInterpreter.liveHandles > before)
      s1.area.toMicroarcsecondsSquared + a2.toMicroarcsecondsSquared
    }
    assert(area > 0)
    assertEquals(WasmShapeInterpreter.liveHandles, before)
    intercept[IllegalStateException](leaked.area)
    assert(!WasmShapeInterpreter.inScope)
  }

  test("scoped frees on failure and free() is idempotent") {
    val w      = kernel()
    val before = WasmShapeInterpreter.liveHandles
    intercept[RuntimeException](w.scoped { w.interpret(rect); throw new RuntimeException("boom") })
    assertEquals(WasmShapeInterpreter.liveHandles, before)
    val s = w.interpret(rect).asInstanceOf[WasmShape]
    assertEquals(WasmShapeInterpreter.liveHandles, before + 1)
    s.free()
    s.free()
    assertEquals(WasmShapeInterpreter.liveHandles, before)
    intercept[IllegalStateException](s.contains(µas(0, 0)))
  }

  test("interpreting inside scoped leaves no intermediate handles behind") {
    val w      = kernel()
    val before = WasmShapeInterpreter.liveHandles
    w.scoped {
      w.interpret(overlay)
      w.interpret(BoundingBox(Union(overlay, Rotate(overlay, Angle.Angle180))))
      assertEquals(WasmShapeInterpreter.liveHandles, before + 2)
    }
    assertEquals(WasmShapeInterpreter.liveHandles, before)
  }
}
