// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * The IFU offset shifts the patrol field and the probe arm together, so anything fixed relative to
 * the field must see the same arm. It is easy to get wrong because OCS applies the offset in two
 * different frames -- the shape frame for the field, sky `p` for the arm -- and because every
 * non-IFU FPU has a zero offset, which hides a sign error on all the other modes.
 *
 * That zero is why imaging and MOS are pinned here too: they share `patrolFieldAtBase`, so a change
 * meant for the IFU can reach them, and nothing about their own geometry would flag it.
 */
class GmosOiwfsGeometrySuite extends munit.FunSuite:

  private val Port: PortDisposition = PortDisposition.Side

  private def arcsec(a: Angle): Double =
    Angle.signedDecimalArcseconds.get(a).toDouble

  // Angle is modular, so a midpoint has to be taken in signed arcseconds: bisecting a small
  // negative angle bisects its 360-degree representation instead and lands half a turn away.
  private def midpoint(a: Angle, b: Angle): Angle =
    Angle.fromDoubleArcseconds((arcsec(a) + arcsec(b)) / 2.0)

  private def box(e: lucuma.core.geom.ShapeExpression): (Double, Double, Double, Double) =
    val b = e.eval.boundingOffsets
    (arcsec(b.topLeft.p.toAngle),     arcsec(b.bottomRight.p.toAngle),
     arcsec(b.bottomRight.q.toAngle), arcsec(b.topLeft.q.toAngle))

  private def assertBox(
    actual:   (Double, Double, Double, Double),
    expected: (Double, Double, Double, Double),
    clue:     String
  ): Unit =
    val (ap, bp, aq, bq) = actual
    val (cp, dp, cq, dq) = expected
    assertEqualsDouble(ap, cp, 0.01, s"$clue max p")
    assertEqualsDouble(bp, dp, 0.01, s"$clue min p")
    assertEqualsDouble(aq, cq, 0.01, s"$clue min q")
    assertEqualsDouble(bq, dq, 0.01, s"$clue max q")

  /** Centre of the patrol field, at the base position and a zero position angle. */
  private def patrolFieldCentre(fpu: Either[GmosNorthFpu, GmosSouthFpu]): Offset =
    val b = oiwfs.patrolField.fpuMode
              .patrolFieldAt(Angle.Angle0, Offset.Zero, fpu, Port)
              .eval.boundingOffsets
    Offset(
      Offset.P(midpoint(b.topLeft.p.toAngle, b.bottomRight.p.toAngle)),
      Offset.Q(midpoint(b.topLeft.q.toAngle, b.bottomRight.q.toAngle))
    )

  /** The arm reaching a field-centred guide star, expressed relative to that guide star. */
  private def armAtFieldCentre(
    fpu: Either[GmosNorthFpu, GmosSouthFpu]
  ): (Double, Double, Double, Double) =
    val centre = patrolFieldCentre(fpu)
    val b      = (oiwfs.probeArm.fpuMode
                   .shapeAt(Angle.Angle0, centre, Offset.Zero, fpu, Port) ↗ (Offset.Zero - centre))
                   .eval.boundingOffsets
    (arcsec(b.topLeft.p.toAngle),     arcsec(b.topLeft.q.toAngle),
     arcsec(b.bottomRight.p.toAngle), arcsec(b.bottomRight.q.toAngle))

  private def assertSameArm(
    fpu:      Either[GmosNorthFpu, GmosSouthFpu],
    baseline: Either[GmosNorthFpu, GmosSouthFpu]
  ): Unit =
    val (ap, aq, bp, bq) = armAtFieldCentre(fpu)
    val (cp, cq, dp, dq) = armAtFieldCentre(baseline)
    assertEqualsDouble(ap, cp, 0.01, s"$fpu p (top left)")
    assertEqualsDouble(aq, cq, 0.01, s"$fpu q (top left)")
    assertEqualsDouble(bp, dp, 0.01, s"$fpu p (bottom right)")
    assertEqualsDouble(bq, dq, 0.01, s"$fpu q (bottom right)")

  test("north IFU apertures reach a field-centred guide star exactly as a slit does"):
    List(GmosNorthFpu.Ifu2Slits, GmosNorthFpu.IfuBlue, GmosNorthFpu.IfuRed)
      .foreach(f => assertSameArm(f.asLeft, GmosNorthFpu.LongSlit_1_00.asLeft))

  test("south IFU apertures reach a field-centred guide star exactly as a slit does"):
    List(GmosSouthFpu.Ifu2Slits,   GmosSouthFpu.IfuBlue,   GmosSouthFpu.IfuRed,
         GmosSouthFpu.IfuNS2Slits, GmosSouthFpu.IfuNSBlue, GmosSouthFpu.IfuNSRed)
      .foreach(f => assertSameArm(f.asRight, GmosSouthFpu.LongSlit_1_00.asRight))

  // The patrol field itself must still move: it is the arm's view of it that is invariant.
  test("the patrol field shifts by the aperture's offset"):
    val slit = patrolFieldCentre(GmosNorthFpu.LongSlit_1_00.asLeft)
    List(GmosNorthFpu.Ifu2Slits, GmosNorthFpu.IfuBlue, GmosNorthFpu.IfuRed).foreach: f =>
      val ifu = patrolFieldCentre(f.asLeft)
      assertEqualsDouble(
        arcsec(ifu.p.toAngle) - arcsec(slit.p.toAngle),
        -arcsec(f.xOffset),
        0.01,
        s"$f p"
      )
      assertEqualsDouble(arcsec(ifu.q.toAngle), arcsec(slit.q.toAngle), 0.01, s"$f q")

  // Imaging and MOS share `patrolFieldAtBase` with the FPU modes, passing a zero offset.  Nothing
  // above would notice if that path picked up a stray sign, so pin it to the OCS rectangle:
  // `GmosOiwfsGuideProbe` declares (-11.4, -34.92, 212.7, 249.6) in the shape frame, where
  // `x = -p` and `y = -q`, and the side-looking port mirrors q.
  test("the imaging patrol field is the OCS rectangle"):
    assertBox(
      box(oiwfs.patrolField.imagingMode.patrolFieldAt(Angle.Angle0, Offset.Zero, PortDisposition.Side)),
      (11.40, -201.30, -34.92, 214.68),
      "side looking"
    )
    assertBox(
      box(oiwfs.patrolField.imagingMode.patrolFieldAt(Angle.Angle0, Offset.Zero, PortDisposition.Bottom)),
      (11.40, -201.30, -214.68, 34.92),
      "up looking"
    )

  // A zero-offset FPU must reach the same field as imaging: both go through the same helper, and
  // negating the offset on only one of the two paths would part them.
  test("a slit patrol field is the imaging patrol field"):
    List(PortDisposition.Side, PortDisposition.Bottom).foreach: port =>
      assertBox(
        box(oiwfs.patrolField.fpuMode
              .patrolFieldAt(Angle.Angle0, Offset.Zero, GmosNorthFpu.LongSlit_1_00.asLeft, port)),
        box(oiwfs.patrolField.imagingMode.patrolFieldAt(Angle.Angle0, Offset.Zero, port)),
        s"$port"
      )
