// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.geom.BoundingOffsets
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.geom.visitors.MaroonXScienceFov
import lucuma.core.geom.visitors.MaroonXSkyFiberPatrol
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

/**
 * `posCalculations` evaluates `probeArmShape` once and places it per guide star with
 * `Shape.transform`. That placed shape must be the very shape `probeArm` describes, for every
 * `AgsParams` variant, position angle, offset and guide star.
 */
class ProbeArmPlacementSuite extends munit.FunSuite:

  private type Params = AgsParams & SingleProbeAgsParams

  private def off(p: Double, q: Double): Offset =
    Offset(Offset.P(Angle.fromDoubleArcseconds(p)), Offset.Q(Angle.fromDoubleArcseconds(q)))

  private val posAngles: List[Angle] =
    List(0.0, 37.5, 90.0, 145.0, 210.0, 300.0).map(Angle.fromDoubleDegrees)

  private val offsets: List[Offset] =
    List(Offset.Zero, off(10, -5), off(-45.25, 30.5))

  private def withProbes[A <: Params & PwfsSupport[A]](name: String, a: A): List[(String, Params)] =
    val pwfs2 = a.probe match
      case GuideProbe.PWFS2 => Nil
      case _                => List(s"$name PWFS2" -> a.withPWFS2)
    List(name -> a, s"$name PWFS1" -> a.withPWFS1) ++ pwfs2

  private val variants: List[(String, Params)] =
    withProbes("GMOS imaging side", AgsParams.GmosImaging(PortDisposition.Side)) ++
      withProbes("GMOS imaging bottom", AgsParams.GmosImaging(PortDisposition.Bottom)) ++
      withProbes("GMOS-N long slit 1.0", AgsParams.GmosLongSlit(GmosNorthFpu.LongSlit_1_00.asLeft)) ++
      withProbes(
        "GMOS-S long slit 0.5 bottom",
        AgsParams.GmosLongSlit(GmosSouthFpu.LongSlit_0_50.asRight, PortDisposition.Bottom)
      ) ++
      withProbes("GMOS-N N&S 0.5", AgsParams.GmosLongSlit(GmosNorthFpu.Ns1.asLeft)) ++
      withProbes("GMOS-N IFU-2", AgsParams.GmosIfu(GmosNorthIfuFpu.TwoSlits.asLeft)) ++
      withProbes(
        "GMOS-S IFU-R bottom",
        AgsParams.GmosIfu(GmosSouthIfuFpu.OneSlitRed.asRight, PortDisposition.Bottom)
      ) ++
      withProbes("GMOS-N MOS", AgsParams.GmosMos(Site.GN)) ++
      withProbes("GMOS-S MOS bottom", AgsParams.GmosMos(Site.GS, PortDisposition.Bottom)) ++
      withProbes(
        "F2 imaging f/16 side",
        AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.F16, PortDisposition.Side)
      ) ++
      withProbes(
        "F2 imaging GeMS-under bottom",
        AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.GemsUnder, PortDisposition.Bottom)
      ) ++
      withProbes(
        "F2 imaging GeMS-over bottom",
        AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.GemsOver, PortDisposition.Bottom)
      ) ++
      withProbes(
        "F2 long slit 2px",
        AgsParams.Flamingos2LongSlit(
          Flamingos2LyotWheel.F16,
          Flamingos2FpuMask.Builtin(Flamingos2Fpu.LongSlit2),
          PortDisposition.Side
        )
      ) ++
      withProbes("F2 MOS", AgsParams.Flamingos2Mos(Flamingos2LyotWheel.F16, PortDisposition.Side)) ++
      withProbes("IGRINS-2", AgsParams.Igrins2LongSlit()) ++
      withProbes(
        "GNIRS long slit",
        AgsParams.GnirsLongSlit(GnirsFpuSlit.LongSlit_0_30, GnirsCamera.ShortBlue, GnirsPrism.Mirror)
      ) ++
      withProbes("GNIRS imaging keyhole", AgsParams.GnirsImaging(GnirsCamera.LongRed, GnirsFilter.Order4)) ++
      withProbes("GNIRS IFU", AgsParams.GnirsIfu(GnirsFpuIfu.LowResolution)) ++
      withProbes("GHOST", AgsParams.GhostIfu()) ++
      withProbes("MaroonX", AgsParams.Visitor(MaroonXSkyFiberPatrol, MaroonXScienceFov)) ++
      withProbes("Visitor 30/10", AgsParams.Visitor(30.arcsec, 10.arcsec))

  private def µas(a: Angle): Long = Angle.signedMicroarcseconds.get(a)

  // Candidate guide stars: the centre and inner quartiles of the patrol field's bounding box.
  private def candidates(b: BoundingOffsets): List[Offset] =
    val pMin = µas(b.bottomRight.p.toAngle)
    val qMin = µas(b.bottomRight.q.toAngle)
    val pw   = (µas(b.topLeft.p.toAngle) - pMin).toDouble
    val qw   = (µas(b.topLeft.q.toAngle) - qMin).toDouble
    List((0.5, 0.5), (0.25, 0.25), (0.75, 0.25), (0.25, 0.75), (0.75, 0.75)).map: (fp, fq) =>
      Offset(
        Offset.P(Angle.fromMicroarcseconds(pMin + (pw * fp).round)),
        Offset.Q(Angle.fromMicroarcseconds(qMin + (qw * fq).round))
      )

  // lucuma-jts cannot build the F2 GeMS arm (TopologyException); those cases are skipped, not failed.
  private def evaluated(e: ShapeExpression): Option[Shape] =
    try Some(e.eval)
    catch case _: org.locationtech.jts.geom.TopologyException => None

  variants.foreach: (name, params) =>
    test(s"$name: placed probe arm equals the evaluated probe arm expression"):
      for
        arm    <- evaluated(params.probeArmShape).toList
        pa     <- posAngles
        offset <- offsets
        gs     <- candidates(params.patrolFieldAt(pa, offset).eval.boundingOffsets)
        direct <- evaluated(params.probeArm(pa, gs, offset))
      do
        val clue   = s"$name PA ${pa.toDoubleDegrees} offset $offset guide star $gs"
        val placed = arm.transform(params.probeArmAngle(pa, gs, offset), gs)
        assertEquals(placed.area, direct.area, s"$clue area")
        assertEquals(placed.boundingOffsets, direct.boundingOffsets, s"$clue bbox")
