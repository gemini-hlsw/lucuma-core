// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import lucuma.core.enums.GmosNorthIfuFpu
import lucuma.core.enums.GmosSouthIfuFpu
import lucuma.core.enums.Site
import lucuma.core.geom.ScienceAreaGeometrySuite
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Offset

class GmosScienceAreaGeometrySuite extends ScienceAreaGeometrySuite:

  private def ifuSides(fieldWidth: Angle): (Angle, Angle) =
    sides(scienceArea.ifuMode.shapeAt(Angle.Angle0, Offset.Zero, fieldWidth))

  // OCS GmosScienceAreaGeometry.IFUFOVLargerRectangle: the target lenslet field the OT draws is
  // 7.5" x 5"; masking to one pseudo-slit halves it across p, leaving 3.75" x 5".
  test("two-slit IFU science area is the full target field (7.5\" x 5\")"):
    List(GmosNorthIfuFpu.TwoSlits.fieldWidth, GmosSouthIfuFpu.TwoSlits.fieldWidth).foreach: w =>
      val (p, q) = ifuSides(w)
      assertCloseArcsec(p, 7.5)
      assertCloseArcsec(q, 5.0)

  test("either one-slit IFU science area is half the target field (3.75\" x 5\")"):
    List(GmosNorthIfuFpu.OneSlitRed.fieldWidth,  GmosNorthIfuFpu.OneSlitBlue.fieldWidth,
         GmosSouthIfuFpu.OneSlitRed.fieldWidth,  GmosSouthIfuFpu.OneSlitBlue.fieldWidth).foreach: w =>
      val (p, q) = ifuSides(w)
      assertCloseArcsec(p, 3.75)
      assertCloseArcsec(q, 5.0)

  private def skySides(fpu: GmosNorthIfuFpu, site: Site): (Angle, Angle) =
    sides(
      scienceArea.ifuMode
        .skyShapeAt(Angle.Angle0, Offset.Zero, fpu.fieldWidth, fpu.skyFieldWidth, site)
    )

  private def skyCentreArcsecP(fieldWidth: Angle, skyFieldWidth: Angle, site: Site): Double =
    val b = scienceArea.ifuMode
      .skyShapeAt(Angle.Angle0, Offset.Zero, fieldWidth, skyFieldWidth, site)
      .eval
      .boundingOffsets
    (b.topLeft.p.toAngle.toSignedDoubleDegrees + b.bottomRight.p.toAngle.toSignedDoubleDegrees) / 2.0 * 3600.0

  // OCS GmosScienceAreaGeometry.IFUFOVSmallerRectangle. The sky field is a separate aperture, not
  // half of the target field: 3.5" for two slits rather than the 3.75" a bisection would give.
  test("sky field is the dedicated sky aperture, not half the target field"):
    val (p2, q2) = skySides(GmosNorthIfuFpu.TwoSlits, Site.GN)
    assertCloseArcsec(p2, 3.5)
    assertCloseArcsec(q2, 5.0)
    val (p1, q1) = skySides(GmosNorthIfuFpu.OneSlitRed, Site.GN)
    assertCloseArcsec(p1, 1.75)
    assertCloseArcsec(q1, 5.0)

  // OCS states these in the shape frame where x = -p, so its -62" for North is +62" here.
  // Getting the sign backwards would put an observer's sky bundle on the wrong patch of sky.
  test("sky field sits 62\" away, East in North and West in South"):
    val n = GmosNorthIfuFpu.TwoSlits
    val s = GmosSouthIfuFpu.TwoSlits
    assertEqualsDouble(skyCentreArcsecP(n.fieldWidth, n.skyFieldWidth, Site.GN), 62.0, 0.01)
    assertEqualsDouble(skyCentreArcsecP(s.fieldWidth, s.skyFieldWidth, Site.GS), -62.0, 0.01)

  test("one-slit sky field shifts in with the narrower fields"):
    val n = GmosNorthIfuFpu.OneSlitRed
    val s = GmosSouthIfuFpu.OneSlitRed
    assertEqualsDouble(skyCentreArcsecP(n.fieldWidth, n.skyFieldWidth, Site.GN), 61.0, 0.01)
    assertEqualsDouble(skyCentreArcsecP(s.fieldWidth, s.skyFieldWidth, Site.GS), -61.0, 0.01)
