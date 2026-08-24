// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import lucuma.core.enums.GmosNorthIfuFpu
import lucuma.core.enums.GmosSouthIfuFpu
import lucuma.core.enums.Site
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Offset

class GmosScienceAreaGeometrySuite extends munit.FunSuite:

  private def sides(shape: ShapeExpression): (Angle, Angle) =
    val b = shape.eval.boundingOffsets
    (b.topLeft.p.toAngle.difference(b.bottomRight.p.toAngle),
     b.topLeft.q.toAngle.difference(b.bottomRight.q.toAngle)
    )

  private def ifuSides(fieldWidth: Angle): (Angle, Angle) =
    sides(scienceArea.ifuMode.shapeAt(Angle.Angle0, Offset.Zero, fieldWidth))

  private def assertCloseArcsec(actual: Angle, expectedArcsec: Double): Unit =
    assertEqualsDouble(actual.toSignedDoubleDegrees * 3600.0, expectedArcsec, 0.01)

  // GmosCommonType.IFU_FOV: the target lenslet field is 7" x 5"; masking to one
  // pseudo-slit halves it across p, leaving 3.5" x 5".
  test("two-slit IFU science area is the full target field (7\" x 5\")"):
    List(GmosNorthIfuFpu.TwoSlits.fieldWidth, GmosSouthIfuFpu.TwoSlits.fieldWidth).foreach: w =>
      val (p, q) = ifuSides(w)
      assertCloseArcsec(p, 7.0)
      assertCloseArcsec(q, 5.0)

  test("either one-slit IFU science area is half the target field (3.5\" x 5\")"):
    List(GmosNorthIfuFpu.OneSlitRed.fieldWidth,  GmosNorthIfuFpu.OneSlitBlue.fieldWidth,
         GmosSouthIfuFpu.OneSlitRed.fieldWidth,  GmosSouthIfuFpu.OneSlitBlue.fieldWidth).foreach: w =>
      val (p, q) = ifuSides(w)
      assertCloseArcsec(p, 3.5)
      assertCloseArcsec(q, 5.0)

  private def skySides(fieldWidth: Angle, site: Site): (Angle, Angle) =
    sides(scienceArea.ifuMode.skyShapeAt(Angle.Angle0, Offset.Zero, fieldWidth, site))

  private def skyCentreArcsecP(fieldWidth: Angle, site: Site): Double =
    val b = scienceArea.ifuMode.skyShapeAt(Angle.Angle0, Offset.Zero, fieldWidth, site).eval.boundingOffsets
    (b.topLeft.p.toAngle.toSignedDoubleDegrees + b.bottomRight.p.toAngle.toSignedDoubleDegrees) / 2.0 * 3600.0

  // The sky bundle is half the width of the target bundle: one pseudo-slit samples half of each.
  test("sky field is half the width of the target field"):
    val (p2, q2) = skySides(GmosNorthIfuFpu.TwoSlits.fieldWidth, Site.GN)
    assertCloseArcsec(p2, 3.5)
    assertCloseArcsec(q2, 5.0)
    val (p1, q1) = skySides(GmosNorthIfuFpu.OneSlitRed.fieldWidth, Site.GN)
    assertCloseArcsec(p1, 1.75)
    assertCloseArcsec(q1, 5.0)

  // OCS GmosScienceAreaGeometry.ifuFOV, OT-10: South mirrors the sky bundle about the base.
  // Getting this backwards would put an observer's sky bundle on the wrong patch of sky.
  test("sky field sits ~62\" away, on opposite sides in North and South"):
    assertEqualsDouble(skyCentreArcsecP(GmosNorthIfuFpu.TwoSlits.fieldWidth, Site.GN), -61.75, 0.01)
    assertEqualsDouble(skyCentreArcsecP(GmosSouthIfuFpu.TwoSlits.fieldWidth, Site.GS), 61.75, 0.01)

  test("one-slit sky field shifts in with the narrower fields"):
    assertEqualsDouble(skyCentreArcsecP(GmosNorthIfuFpu.OneSlitRed.fieldWidth, Site.GN), -60.875, 0.01)
    assertEqualsDouble(skyCentreArcsecP(GmosSouthIfuFpu.OneSlitRed.fieldWidth, Site.GS), 60.875, 0.01)
