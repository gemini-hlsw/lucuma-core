// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.geom.visitors.MaroonXScienceFov
import lucuma.core.geom.visitors.MaroonXSkyFiberPatrol
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

/** The `AgsParams` variants the placement suites run over, with the angles and offsets to try. */
object AgsParamsVariants:

  type Params = AgsParams & SingleProbeAgsParams

  def off(p: Double, q: Double): Offset =
    Offset(Offset.P(Angle.fromDoubleArcseconds(p)), Offset.Q(Angle.fromDoubleArcseconds(q)))

  val posAngles: List[Angle] =
    List(0.0, 37.5, 90.0, 145.0, 210.0, 300.0).map(Angle.fromDoubleDegrees)

  val offsets: List[Offset] =
    List(Offset.Zero, off(10, -5), off(-45.25, 30.5))

  def withProbes[A <: Params & PwfsSupport[A]](name: String, a: A): List[(String, Params)] =
    val pwfs2 = a.probe match
      case GuideProbe.PWFS2 => Nil
      case _                => List(s"$name PWFS2" -> a.withPWFS2)
    List(name -> a, s"$name PWFS1" -> a.withPWFS1) ++ pwfs2

  val variants: List[(String, Params)] =
    withProbes("GMOS imaging side", AgsParams.GmosImaging(PortDisposition.Side)) ++
      withProbes("GMOS imaging bottom", AgsParams.GmosImaging(PortDisposition.Bottom)) ++
      withProbes("GMOS-N long slit 1.0",
                 AgsParams.GmosLongSlit(GmosNorthFpu.LongSlit_1_00.asLeft)
      ) ++
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
      withProbes("F2 MOS",
                 AgsParams.Flamingos2Mos(Flamingos2LyotWheel.F16, PortDisposition.Side)
      ) ++
      withProbes("IGRINS-2", AgsParams.Igrins2LongSlit()) ++
      withProbes(
        "GNIRS long slit",
        AgsParams.GnirsLongSlit(GnirsFpuSlit.LongSlit_0_30,
                                GnirsCamera.ShortBlue,
                                GnirsPrism.Mirror
        )
      ) ++
      withProbes("GNIRS imaging keyhole",
                 AgsParams.GnirsImaging(GnirsCamera.LongRed, GnirsFilter.Order4)
      ) ++
      withProbes("GNIRS IFU", AgsParams.GnirsIfu(GnirsFpuIfu.LowResolution)) ++
      List(
        "GNIRS imaging Altair NGS"      ->
          AgsParams
            .GnirsImaging(GnirsCamera.ShortBlue, GnirsFilter.Order4)
            .withAltair(AltairMode.Ngs),
        "GNIRS long slit Altair LGS+P1" ->
          AgsParams
            .GnirsLongSlit(GnirsFpuSlit.LongSlit_0_30, GnirsCamera.ShortBlue, GnirsPrism.Mirror)
            .withAltair(AltairMode.LgsP1)
      ) ++
      withProbes("GHOST", AgsParams.GhostIfu()) ++
      withProbes("MaroonX", AgsParams.Visitor(MaroonXSkyFiberPatrol, MaroonXScienceFov)) ++
      withProbes("Visitor 30/10", AgsParams.Visitor(30.arcsec, 10.arcsec))
