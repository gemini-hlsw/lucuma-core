// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.syntax.all.*
import lucuma.ags.AgsParams.AltairSupport
import lucuma.catalog.BandsList
import lucuma.catalog.BrightnessConstraints
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.SkyBackground
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.util.arb.ArbEnumerated.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class AltairAgsParamsSuite extends ScalaCheckSuite:

  private val shortestWavelength: Wavelength = Wavelength.fromIntNanometers(300).get

  private def checkAltair[A <: AgsParams & SingleProbeAgsParams & AltairSupport[A]](
    params: A,
    mode:   AltairMode
  ): Unit =
    val result: A = params.guidedBy(mode.guideProbe.some, mode.some)
    assertEquals(result.probe, mode.guideProbe)
    assertEquals(result.altair, mode.some)

  private def checkPwfs[A <: AgsParams & SingleProbeAgsParams & AltairSupport[A]](
    params: A,
    mode:   AltairMode
  ): Unit =
    val result: A = params.guidedBy(GuideProbe.PWFS2.some, mode.some)
    assertEquals(result.probe, GuideProbe.PWFS2)
    assertEquals(result.altair, none)

  property("GNIRS long slit behind Altair uses the Altair mode's probe"):
    forAll: (fpu: GnirsFpuSlit, camera: GnirsCamera, prism: GnirsPrism, mode: AltairMode) =>
      checkAltair(AgsParams.GnirsLongSlit(fpu, camera, prism), mode)

  property("GNIRS imaging behind Altair uses the Altair mode's probe"):
    forAll: (camera: GnirsCamera, filter: GnirsFilter, mode: AltairMode) =>
      checkAltair(AgsParams.GnirsImaging(camera, filter), mode)

  property("GNIRS long slit on another PWFS guides without Altair"):
    forAll: (fpu: GnirsFpuSlit, camera: GnirsCamera, prism: GnirsPrism, mode: AltairMode) =>
      checkPwfs(AgsParams.GnirsLongSlit(fpu, camera, prism).withPWFS1, mode)

  property("GNIRS imaging on another PWFS guides without Altair"):
    forAll: (camera: GnirsCamera, filter: GnirsFilter, mode: AltairMode) =>
      checkPwfs(AgsParams.GnirsImaging(camera, filter).withPWFS1, mode)

  property("LGS+P1 guides with PWFS1 and keeps the Altair mode"):
    forAll: (fpu: GnirsFpuSlit, camera: GnirsCamera, prism: GnirsPrism) =>
      val result: AgsParams.GnirsLongSlit =
        AgsParams
          .GnirsLongSlit(fpu, camera, prism)
          .guidedBy(GuideProbe.PWFS1.some, AltairMode.LgsP1.some)
      assertEquals(result.probe, GuideProbe.PWFS1)
      assertEquals(result.altair, AltairMode.LgsP1.some)

  property("No probe and no Altair leaves the params unchanged"):
    forAll: (fpu: GnirsFpuSlit, camera: GnirsCamera, prism: GnirsPrism, mode: AltairMode) =>
      val params: AgsParams.GnirsLongSlit =
        AgsParams.GnirsLongSlit(fpu, camera, prism).withAltair(mode)
      assertEquals(params.guidedBy(none, none), params)

  property("widestAltairConstraints covers every Altair limit"):
    forAll:
      (
        mode:  AltairMode,
        speed: GuideSpeed,
        sb:    SkyBackground,
        iq:    ImageQuality.Preset,
        ce:    CloudExtinction.Preset
      ) =>
        val constraints: BrightnessConstraints =
          altairBrightnessConstraints(
            mode,
            speed,
            shortestWavelength,
            sb,
            iq.toImageQuality,
            ce.toCloudExtinction
          )
        assertEquals(widestAltairConstraints.searchBands, BandsList.RBandsList)
        assert(
          widestAltairConstraints.faintnessConstraint >= constraints.faintnessConstraint
        )
        assert(
          (widestAltairConstraints.saturationConstraint, constraints.saturationConstraint)
            .mapN(_ <= _)
            .getOrElse(false)
        )

  property("widestConstraintsFor uses the Altair limits only for the AOWFS"):
    forAll: (probe: GuideProbe) =>
      val expected: BrightnessConstraints =
        probe match
          case GuideProbe.AltairAOWFS => widestAltairConstraints
          case _                      => widestConstraints
      assertEquals(widestConstraintsFor(probe), expected)
