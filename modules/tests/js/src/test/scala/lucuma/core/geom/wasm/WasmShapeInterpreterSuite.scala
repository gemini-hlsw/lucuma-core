// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import cats.effect.IO
import cats.effect.Resource
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeExpression.*
import lucuma.core.geom.ShapePolygon
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.jts.JtsShapeInterpreter
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import munit.CatsEffectSuite


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

  test("load installs the kernel as the default interpreter") {
    assertEquals(kernel(), WasmShapeInterpreter: ShapeInterpreter)
    assertEquals(ShapeInterpreter.default, WasmShapeInterpreter: ShapeInterpreter)
    assert(WasmGeometry.isCompatible(LucumaGeoWasm.version()), LucumaGeoWasm.version())
  }

  test("memoryBytes reports the kernel's linear memory once loaded") {
    kernel()
    val bytes = WasmShapeInterpreter.memoryBytes
    assert(bytes > 0, bytes)
    assertEquals(bytes % 65536, 0L, "wasm memory is a whole number of 64 KiB pages")
  }

  test("version range check") {
    assert(WasmGeometry.isCompatible("0.1.0"))
    assert(WasmGeometry.isCompatible("0.1.7-rc1"))
    assert(!WasmGeometry.isCompatible("0.2.0"))
    assert(!WasmGeometry.isCompatible("1.0.0"))
    assert(!WasmGeometry.isCompatible("garbage"))
  }

  test("polygons round-trip a shape with a hole and two parts") {
    val hole  = Rectangle(µas(-2 * Arcsec, -2 * Arcsec), µas(2 * Arcsec, 2 * Arcsec))
    val far   = Translate(hole, µas(30 * Arcsec, 30 * Arcsec))
    val shape = Union(Difference(rect, hole), far)
    val w     = kernel().interpret(shape)
    val j     = jts(shape)
    assertEquals(w.polygons.length, 2)
    assertEquals(w.polygons.map(_.holes.length).sorted, List(0, 1))
    assertEquals(j.polygons.length, 2)
    assertEquals(j.polygons.map(_.holes.length).sorted, List(0, 1))
    // Rebuilding from the vertices draws the same figure on either engine.
    val rebuilt = ShapePolygon.toShapeExpression(w.polygons)
    assertClose(
      jts(rebuilt).area.toMicroarcsecondsSquared.toDouble,
      j.area.toMicroarcsecondsSquared.toDouble,
      1e-7,
      "rebuilt area"
    )
    val o = µas(5 * Arcsec, 0)
    assert(jts(rebuilt).contains(o))
    assert(!jts(rebuilt).contains(Offset.Zero))
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
