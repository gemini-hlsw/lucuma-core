// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gnirs

import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Offset

class GnirsScienceAreaGeometrySuite extends munit.FunSuite:

  private def sides(shape: lucuma.core.geom.ShapeExpression): (Angle, Angle) =
    val b = shape.eval.boundingOffsets
    (b.topLeft.p.toAngle.difference(b.bottomRight.p.toAngle),
     b.topLeft.q.toAngle.difference(b.bottomRight.q.toAngle)
    )

  // Bounding-box side lengths (p, q) of the imaging science area at PA 0, no offset.
  private def imagingSides(camera: GnirsCamera, filter: GnirsFilter): (Angle, Angle) =
    sides(scienceArea.imagingShapeAt(Angle.Angle0, Offset.Zero, camera, filter))

  private def ifuSides(ifu: GnirsFpuIfu): (Angle, Angle) =
    sides(scienceArea.ifuShapeAt(Angle.Angle0, Offset.Zero, ifu))

  private def assertCloseArcsec(actual: Angle, expectedArcsec: Double): Unit =
    assertEqualsDouble(actual.toSignedDoubleDegrees * 3600.0, expectedArcsec, 0.01)

  // Order4 (H) is a keyhole filter; the bar runs the full no-XD slit length along q, and
  // the cap bumps out 10" along p, so the bounding box is (p = 10 + 10) x (q = slit length).
  test("keyhole imaging bounding box is the full bar for the short camera (20\" x 99\")"):
    List(GnirsCamera.ShortBlue, GnirsCamera.ShortRed).foreach: camera =>
      val (p, q) = imagingSides(camera, GnirsFilter.Order4)
      assertCloseArcsec(p, 20.0)
      assertCloseArcsec(q, 99.0)

  test("keyhole imaging bounding box is the full bar for the long camera (20\" x 49\")"):
    List(GnirsCamera.LongBlue, GnirsCamera.LongRed).foreach: camera =>
      val (p, q) = imagingSides(camera, GnirsFilter.Order4)
      assertCloseArcsec(p, 20.0)
      assertCloseArcsec(q, 49.0)

  // Y/J/K-MK filters see the smaller round field: 9" (p) x 24" (q) rectangle plus a 7"
  // cap along p, camera-independent.
  test("round imaging bounding box is the MK field (16\" x 24\"), independent of camera"):
    for
      filter <- List(GnirsFilter.Y, GnirsFilter.J, GnirsFilter.K)
      camera <- GnirsCamera.values.toList
    do
      val (p, q) = imagingSides(camera, filter)
      assertCloseArcsec(p, 16.0)
      assertCloseArcsec(q, 24.0)

  test("LR-IFU science area is the IFU slit width by the low-res height (3.15\" x 4.8\")"):
    val (w, h) = ifuSides(GnirsFpuIfu.LowResolution)
    assertCloseArcsec(w, 3.15)
    assertCloseArcsec(h, 4.8)

  test("HR-IFU science area is the IFU slit width by the high-res height (1.25\" x 1.8\")"):
    val (w, h) = ifuSides(GnirsFpuIfu.HighResolution)
    assertCloseArcsec(w, 1.25)
    assertCloseArcsec(h, 1.8)
